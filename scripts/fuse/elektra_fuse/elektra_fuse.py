#!/usr/bin/env python3
import argparse, logging, logging.handlers
from fuse import FUSE
from rootlevel_resolver import RootlevelResolver

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('mountpoint')
    parser.add_argument('-f', '--foreground', default = False)
    parser.add_argument('-l', '--logger', default = "stdout", choices = ["syslog", "stdout", "none"])
    parser.add_argument('-ll', '--loglevel', default = "DEBUG", choices = ["INFO", "DEBUG", "ERROR", "CRITICAL", "FATAL", "WARN"])
    parser.add_argument('-a', '--allow-other', default = True)
    parser.add_argument('-nt', '--nothreads', default = True)
    args = parser.parse_args()


    #configure logging
    logging.basicConfig(level = getattr(logging, args.loglevel))
    logger = logging.getLogger()
    if args.logger == "syslog":
        logger.addHandler(logging.handlers.SysLogHandler(address = '/dev/log'))
    elif args.logger == "none":
        logger.propagate = False
    elif args.logger == "stdout":
        pass

    fuse = FUSE(RootlevelResolver(args.mountpoint), args.mountpoint, foreground = args.foreground, allow_other = args.allow_other, nothreads = args.nothreads)
