#!/usr/bin/python

# try: 
# > kdb set user/key "to remove"
# > python remove.py
# > kdb get user/key "to remove"

from elektra import *
import sys

kdb=Kdb()
kdb.open()
kdb.remove("user/key")

kdb.close()
