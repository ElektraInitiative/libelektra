#!/usr/bin/python

# try: 
# > kdb set user/key "to remove"
# > python remove.py
# > kdb get user/key "to remove"

from libelektra import *
import sys

h=kdbOpen()
kdbRemove(h,"user/key")

kdbClose(h)
