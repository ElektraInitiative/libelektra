#!/usr/bin/env python3
import argparse, logging
from fuse import FUSE
from rootlevel_resolver import RootlevelResolver

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('mountpoint')
    parser.add_argument('-f', '--foreground', default=False)
    parser.add_argument('-a', '--allow-other', default=True)
    parser.add_argument('-nt', '--nothreads', default=True)
    args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG)
    fuse = FUSE(RootlevelResolver(args.mountpoint), args.mountpoint, foreground = args.foreground, allow_other = args.allow_other, nothreads = args.nothreads)
