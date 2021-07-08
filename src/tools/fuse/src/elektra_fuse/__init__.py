import argparse, logging, logging.handlers, sys
from pathlib import Path
from fuse import FUSE
import kdb
from .rootlevel_resolver import RootlevelResolver
from . import elektra_util

if __name__ == '__main__':
	main()

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('mountpoint')
    parser.add_argument('-f', '--foreground', default = False)

    parser.add_argument('-p', '--parent_key', default = "/")

    parser.add_argument('-l', '--logger', default = "stdout", choices = ["syslog", "stdout", "none"])
    parser.add_argument('-ll', '--loglevel', default = "DEBUG", choices = ["INFO", "DEBUG", "ERROR", "CRITICAL", "FATAL", "WARN"])
    parser.add_argument('-a', '--allow-other', default = True)
    parser.add_argument('-nt', '--nothreads', default = True)

    args = parser.parse_args()

    #validate parent_key
    try:
        parent_key = kdb.Key(args.parent_key)
        if not parent_key.isValid() or not parent_key.isCascading():
            raise NameError
        elektra_util.parent_key = parent_key
    except kdb.kdb.KeyInvalidName:
        raise NameError
    except NameError:
        print("parent_key needs to be a valid key in the cascading namespace", file=sys.stderr)
        sys.exit("1")


    #configure logging
    logging.basicConfig(level = getattr(logging, args.loglevel))
    logger = logging.getLogger()
    if args.logger == "syslog":
        logger.addHandler(logging.handlers.SysLogHandler(address = '/dev/log'))
    elif args.logger == "none":
        logger.propagate = False
    elif args.logger == "stdout":
        pass

    FUSE(RootlevelResolver(args.mountpoint), args.mountpoint, foreground = args.foreground, allow_other = args.allow_other, nothreads = args.nothreads)
