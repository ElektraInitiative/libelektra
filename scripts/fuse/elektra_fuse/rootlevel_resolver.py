#!/usr/bin/env python3

import errno, stat, re, os, psutil, logging, subprocess
from pathlib import Path

from fuse import FUSE, FuseOSError, Operations, LoggingMixIn

import mock_context
import elektra_fuse_interface


#fuse references: https://www.cs.hmc.edu/~geoff/classes/hmc.cs135.201109/homework/fuse/fuse_doc.html
#                 https://libfuse.github.io/doxygen/structfuse__operations.html#a729e53d36acc05a7a8985a1a3bbfac1e
#TODO: permission handling, maybe see fuse 'default_permissions'


#decorator to translate OSError to FUSEOSError.
#elektra_fuse_interface does not throw FUSEOSError, because it cannot be pickled
def with_translated_exceptions(func):
    def wrapper(self, *args, **kwargs):
        try:
            return func(self, *args, **kwargs)
        except OSError as os_error:
            raise FuseOSError(*os_error.args) from os_error
    return wrapper


# Implements a FUSE file system
# Calls regarding the root level (upto one level down from the mountpoint) are handled here
# The first level lists all suitable pids ready for inspection (working dir not below mountpoint)
# Below a certain pid, eg ...mountpoint/27/ lies a complete view of the elektra hierachy from the mocked context of that process
# (these calls get executed in another process context and are implemented in elektra_fuse_interface)
class RootlevelResolver(LoggingMixIn, Operations):
    'Elektra as a FUSE-Filesystem'

    def __init__(self, mountpoint):
        self.mountpoint = mountpoint
        self.fd = 0

    #e.g '/' and '/12' are considered rootlevel
    @staticmethod
    def is_rootlevel_path(path):
        return len(Path(path).parts) <= 2

    # resolves a path "/<pid>/suffix" to "/suffix", where pid need to be a valid process id
    # returns the process imformation with the suffix 
    def resolve_proc_path(self, proc_path):
        matches = re.match("^/(\d+)(.*)$", proc_path)
        if matches:
            pid = int(matches.group(1))
            path_suffix = matches.group(2)
            if path_suffix == '':
                path_suffix = "/"
            
            try:
                process_context = mock_context.parse_process_context_of_pid(pid)

                if self._is_path_child_of(process_context.cwd, self.mountpoint): #same rationale as self.readdir
                    raise FileNotFoundError
                
                return process_context, path_suffix
            except FileNotFoundError:
                raise OSError(errno.ENOENT)
        else:
            raise OSError(errno.ENOENT)


    def _exists_in_rootlevel(self, path):
        return path == "/" or Path(path).name in self._get_all_pids()
        #TODO: cannot use self.readdir here, as this triggers an endless recusion. Meanwhile, excluded pids are falsely flagged as existing.
        # (which only leads to unexpected behaviour in case of especially crafted requests)

    
    @with_translated_exceptions
    def getattr(self, path, fh=None):

        generic_dir_attrs = dict(
            st_mode = (stat.S_IFDIR | 0o755),
            st_ctime = elektra_fuse_interface.startup_time,
            st_mtime = elektra_fuse_interface.startup_time,
            st_atime = elektra_fuse_interface.startup_time,
            st_nlink = 2)

        if RootlevelResolver.is_rootlevel_path(path):
            if self._exists_in_rootlevel(path):
                return generic_dir_attrs
            else:
                raise OSError(errno.ENOENT)

        process_context, path_suffix = self.resolve_proc_path(path)

        return mock_context.run_as(process_context, elektra_fuse_interface.getattr, path_suffix, fh=fh)

    def _get_all_pids(self):
        return {file for file in os.listdir("/proc") if file.isnumeric()}

    def _is_path_child_of(self, potential_child_path, potential_ancestor_path):
        potential_child_path = Path(potential_child_path).resolve()
        potential_ancestor_path = Path(potential_ancestor_path).resolve()
        return potential_ancestor_path in [*potential_child_path.parents, potential_child_path]

    @with_translated_exceptions
    def readdir(self, path, fh):

        if path == "/":
            all_pids = self._get_all_pids()

            #filter out processes with working directory under mount point to prevent endless recursions (e.g. an interactive shell navigating below the mount point)
            #excluded_pids = {pid for pid in all_pids if self._is_path_child_of(mock_context.parse_process_context_of_pid(pid).cwd, self.mountpoint)}
            
            excluded_pids = set()
            for pid in all_pids:
                try:
                    pid_cwd_resolved = os.readlink("/proc/%s/cwd" % pid)
                except OSError:
                    #process does not exist any more
                    excluded_pids.add(pid)
                if self._is_path_child_of(pid_cwd_resolved, self.mountpoint):
                    excluded_pids.add(pid)


            displayed_pids = all_pids - excluded_pids

            return ['.', '..', *displayed_pids]

        process_context, path_suffix = self.resolve_proc_path(path)

        return mock_context.run_as(process_context, elektra_fuse_interface.readdir, path_suffix, fh)


    #hand out dummy file descriptors
    def _new_fd(self):
        self.fd += 1
        return self.fd

    @with_translated_exceptions
    def open(self, path, flags):
        #TODO: for correctness sake the existance of the path (e.g. using stat) should be checked
        return self._new_fd() #not used but nessecary

    @with_translated_exceptions
    def read(self, path, size, offset, fh):
        if RootlevelResolver.is_rootlevel_path(path):
            if _exists_in_rootlevel(path):
                raise OSError(errno.EISDIR) #is dir
            else:
                raise OSError(errno.ENOENT) #does not exist

        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.read, path_suffix, size, offset, fh)

    @with_translated_exceptions
    def write(self, path, data, offset, fh):
        if RootlevelResolver.is_rootlevel_path(path):
            if _exists_in_rootlevel(path):
                raise OSError(errno.EROFS) #read-only fs
            else:
                raise OSError(errno.ENOENT) #does not exist
        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.write, path_suffix, data, offset, fh)

    @with_translated_exceptions
    def truncate(self, path, length, fh=None):
        if RootlevelResolver.is_rootlevel_path(path):
            if _exists_in_rootlevel(path):
                raise OSError(errno.EROFS)
            else:
                raise OSError(errno.ENOENT)

        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.truncate, path_suffix, length, fh=fh)

    @with_translated_exceptions
    def create(self, path, mode):
        if RootlevelResolver.is_rootlevel_path(path):
            raise OSError(errno.EROFS) #read only

        process_context, path_suffix = self.resolve_proc_path(path)
        mock_context.run_as(process_context, elektra_fuse_interface.create, path_suffix, mode)
        return self._new_fd()

    @with_translated_exceptions
    def mkdir(self, path, mode):

        if RootlevelResolver.is_rootlevel_path(path):
            raise OSError(errno.EROFS)

        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.mkdir, path_suffix, mode)

    @with_translated_exceptions
    def getxattr(self, path, name, position=0):
        if RootlevelResolver.is_rootlevel_path(path):
            if path == "/":
                raise OSError(errno.ENOENT)
            elif self._exists_in_rootlevel(path):
                pid = int(Path(path).name)
                process_context_dict = mock_context.parse_process_context_of_pid(pid)._asdict()

                if name in process_context_dict:
                    return str(process_context_dict[name]).encode()
                else:
                    raise OSError(errno.ENOENT)

            else:
                raise OSError(errno.ENOENT)
        
        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.getxattr, path_suffix, name, position=position)

    @with_translated_exceptions
    def listxattr(self, path):

        if RootlevelResolver.is_rootlevel_path(path):
            if path == "/":
                return []
            elif self._exists_in_rootlevel(path):
                pid = int(Path(path).name)
                process_context = mock_context.parse_process_context_of_pid(pid)
                return process_context._fields
            else:
                raise OSError(errno.ENOENT)

            return
        
        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.listxattr, path_suffix)

    @with_translated_exceptions
    def removexattr(self, path, name):
        if RootlevelResolver.is_rootlevel_path(path):
            raise OSError(errno.EROFS) #read only fs
        
        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.removexattr, path_suffix, name)

    @with_translated_exceptions
    def setxattr(self, path, name, value, options, position=0):
        if RootlevelResolver.is_rootlevel_path(path):
            raise OSError(errno.EROFS)
        
        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.setxattr, path_suffix, name, value, options, position=position)

    @with_translated_exceptions
    def unlink(self, path):
        if RootlevelResolver.is_rootlevel_path(path):
            raise OSError(errno.EROFS)
        
        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.unlink, path_suffix)

    @with_translated_exceptions
    def rmdir(self, path):
        if RootlevelResolver.is_rootlevel_path(path):
            raise OSError(errno.EROFS)
        
        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.rmdir, path_suffix)

    @with_translated_exceptions
    def rename(self, old_path, new_path):
        if RootlevelResolver.is_rootlevel_path(old_path) or RootlevelResolver.is_rootlevel_path(new_path):
            raise OSError(errno.EROFS)
        
        process_context_old_path, old_path_suffix = self.resolve_proc_path(old_path)
        process_context_new_path, new_path_suffix = self.resolve_proc_path(new_path)

        if process_context_old_path.pid != process_context_new_path.pid:
            raise OSError(errno.EINVAL)
        return mock_context.run_as(process_context_old_path, elektra_fuse_interface.rename, old_path_suffix, new_path_suffix)

    @with_translated_exceptions
    def chmod(self, path, mode):
        if RootlevelResolver.is_rootlevel_path(path):
            raise OSError(errno.EROFS)
        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.chmod, path_suffix, mode)

    @with_translated_exceptions
    def chown(self, path, uid, gid):
        if RootlevelResolver.is_rootlevel_path(path):
            raise OSError(errno.EROFS)
        
        process_context, path_suffix = self.resolve_proc_path(path)
        return mock_context.run_as(process_context, elektra_fuse_interface.chown, path_suffix, mode)
