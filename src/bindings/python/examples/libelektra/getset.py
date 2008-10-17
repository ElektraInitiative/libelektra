#!/usr/bin/python

# try: 
# > python getset.py

from libelektra import *
import sys

ks=ksNew()
h=kdbOpen()

k=keyNew("user/key")
keySetName(k,"key")
keySetString(k,"hi")

kdbSetKey(h,k)

kdbGetChildKeys(h,"user",ks,1)

k=ksLookupByName(ks,"/key",0)
print keyGetString(k)

kdbClose(h)
keyClose(k)
