#!/usr/bin/python

# try: 
# > kdb set user/hello "Hello World!"
# > python hello.py

from libelektra import *
import sys

def BailOut(msg):
	print msg
	print kdbStrError(kdbhGetError(0))
	sys.exit(1)

ks=ksNew()

h=kdbOpen()
if h==0:
	BailOut('Could not open key database')

if kdbGetChildKeys(h,"user",ks,1)==-1:
	BailOut('Could not get keys')

k=ksLookupByName(ks,"/hello",0);
if k==0:
	BailOut('Could not lookup keys')

#print keyStealValue(k)
print keyGetString(k)
ksDel(ks)
kdbClose(h)
