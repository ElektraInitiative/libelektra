#!/usr/bin/python

# try: 
# > kdb set user/myapp "hello world!"
# > kdb set system/myapp "hi world!"
# > python cascading.py

from libelektra import *
import sys

def BailOut(msg):
	print 'msg'
	print kdbStrError(kdbhGetError(0))
	sys.exit(1)

ks=ksNew()

(h,r)=kdbOpen()
if r==-1:
	BailOut('Could not open key database')

if kdbGetChildKeys(h,"user/myapp",ks,0)==-1:
	BailOut('Could not get keys')

if kdbGetChildKeys(h,"system/myapp",ks,0)==-1:
	BailOut('Could not get keys')

k=ksLookupByName(ks,"/myapp/key",0);
if k==0:
	BailOut('Could not lookup keys')

print keyStealValue(k)
ksDel(ks)
kdbClose(h)
