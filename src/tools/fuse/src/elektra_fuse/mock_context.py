import multiprocessing, os, pwd, re, sys, logging, errno
from collections import namedtuple
from pathlib import Path
import kdb

ProcessContext = namedtuple('ProcessContext', ["ruid", "euid", "suid", "fuid", "rgid", "egid", "sgid", "fgid", "env", "argv", "cwd", "pw_dir", "pw_name", "pid"])

def parse_process_context_of_pid(pid):
    #parse user and group ids
    with open("/proc/%d/status" % pid, "r") as f:
        status = f.read()
        ruid, euid, suid, fuid = [int(num) for num in re.findall("^Uid:\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s*$", status, re.MULTILINE)[0]]
        rgid, egid, sgid, fgid = [int(num) for num in re.findall("^Gid:\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s*$", status, re.MULTILINE)[0]]

    #parse environment variables
    with open("/proc/%d/environ" % pid, "r") as f:
        raw_env_str = f.read()
        env = dict( key_value.split('=', 1) for key_value in raw_env_str.strip("\0").split("\0") )

    #parse argv
    with open("/proc/%d/cmdline" % pid, "r") as f:
        cmdline = f.read()
        argv = cmdline.strip("\0").split("\0")

    #parse current working directory
    cwd = Path("/proc/%d/cwd" % pid).resolve()

    struct_passwd = pwd.getpwuid(ruid)
    pw_dir = struct_passwd.pw_dir
    pw_name = struct_passwd.pw_name
    return ProcessContext(ruid, euid, suid, fuid, rgid, egid, sgid, fgid, env, argv, cwd, pw_dir, pw_name, pid)

def _mock_process_context_and_run(process_context, func, args, kwargs):

    #libelekra depends both on $HOME and the real user id (not the effective user id!) to resolve the user: namespace
    #therefore, temporarely changing the effective user id does not work

    #mock cwd
    os.chdir(process_context.cwd)

    #mock environment (minus $HOME)
    os.environ.clear()
    os.environ.update(process_context.env)
    if "HOME" in os.environ:
        del os.environ["HOME"]


    #mock user/group

    #in case euid == ruid, not sure how to set euid without potential PermissionError

    # os.setegid(process_context.egid)
    # os.seteuid(process_context.euid)
    
    os.setgid(process_context.rgid)
    os.setuid(process_context.ruid)

    #mock argv

    sys.argv = process_context.argv

    try:
        return func(*args, **kwargs)
    except kdb.Exception as e:
        exception_message = str(e).lower()

        #some failing legal requests (i.e. elektra behaves as expected and did not face an (unknown) internal error), like an unpriviliged user trying to write to system: have to be parsed
        if "permission denied" in exception_message:
            raise OSError(errno.EACCES)

        #all other errors: log, then translate kdb.Exception to OSError (as SwigPyObject is not picklable).
        logging.exception("An exception in kdb occured.")
        raise OSError(errno.EIO)
    except PermissionError:
        raise OSError(errno.EACCES)

def run_as(process_context, func, *args, **kwargs):
    with multiprocessing.Pool(processes = 1) as pool:
        return pool.starmap(_mock_process_context_and_run, [ [process_context, func, args, kwargs] ])[0]
