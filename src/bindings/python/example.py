#!/usr/bin/python

# !!!BUG!!!
# This python script gets an segmentation fault

from libelektra import *
import sys

CHILD_KEY_ROOT='user/'

def printKey(key):
	n=keyGetNameSize(key)
	nam=keyGetName(key)
	s=keyGetDataSize(key)
	str=keyGetString(key)
	c=keyGetCommentSize(key)
	com=keyGetComment(key)
	print "Name[%d,%d]: %s\t"%(n,len(nam)+1,nam)
	print "String[%d,%d]: %s\t"%(s,len(str)+1,str)
	print "Comment[%d,%d]: %s\t"%(c,len(com)+1,com)

def printKeySet(ks):
	print>>sys.stderr, 'Will print keys'
	ksRewind(ks)
	print>>sys.stderr, 'Get next key'
	k=ksNext(ks)
	while k!=0:
		printKey(k)
		k=ksNext(ks)

def getChildKeys(handle):
	set=ksNew()
	root=keyNew()
	keySetName(root,CHILD_KEY_ROOT)
	ret=kdbGetKeyChildKeys(handle,root,set,0)
	if ret>=0:
		print>>sys.stderr, 'Got all keys'
	else:
		print>>sys.stderr, 'Error in kdbGetChildKeys'

	printKey(set)
	ksDel(set)

print>>sys.stderr, 'start app'
(handle,r)=kdbOpen()
#print handle
print>>sys.stderr, 'after kdbOpen'
getChildKeys(handle)
kdbClose(handle)
