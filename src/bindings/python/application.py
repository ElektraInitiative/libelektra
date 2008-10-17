#!/usr/bin/python

from libelektra import *
import sys

MY_APP_ROOT="system/sw/MyApp/current"

def readConfig(handle,myConfig):
	return kdbGetChildKeys(handle,MY_APP_ROOT,myConfig,0)

def changeConfig(myConfig):
	ksRewind(myConfig)
	current=1
	while True:
		current=ksNext(myConfig)
		if current==0: break
		keyName=keyGetFullName(current)
		value=keyGetString(current)
		value=value+"- modified"
		print 'Key %s was %s'%(keyName,value)
		keySetString(current, value)
		value=keyGetString(current)

def saveConfig(handle,myConfig):
	return kdbSetKeys(handle,myConfig)

myConfig=ksNew()
(handle,r)=kdbOpen()

if readConfig(handle,myConfig)==-1: 
	sys.stderr.write("Couldn't get my configuration. Reason\n")
	sys.exit(1)
else:
	print "retrieved %d keys"%ksGetSize(myConfig)

changeConfig(myConfig)
saveConfig(handle,myConfig)
kdbClose(handle)
ksDel(myConfig)
