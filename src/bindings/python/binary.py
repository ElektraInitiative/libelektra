#!/usr/bin/python

from libelektra import *

key=keyNew()
keySetName(key,"user/tmp/bin")
(h,r)=kdbOpen()
keySetBinary(key,"\000ABC")
kdbSetKey(h,key)
kdbClose(h)
