#!/usr/bin/python

import libpyelektra

class ElektraException(Exception):
	def __init__(self,msg):
		self.msg=msg
	def __str__(self):
		return self.msg

class Kdb:
	"""General methods to acces the key database
"""
	def __init__(self):
		self.__kdb__pointer__=libpyelektra.kdbOpen()
	def __del__(self):
		libpyelektra.kdbClose(self.__kdb__pointer__)

	#def close(self):
	#	"""Closes the backend."""
	#	libpyelektra.kdbClose(self.__kdb__pointer__)

	def getKey(self,key):
		"""Gets key from backend storage."""
		return libpyelektra.kdbGetKey(self.__kdb__pointer__,key.__key__pointer__)
	def getKeyChildKeys(self, key, *args, **kargs):
		options=0
		if 'norecursive' in kargs and kargs['norecursive']: options+=1
		if 'excldir' in kargs and kargs['excldir']: options+=2
		if 'dironly' in kargs and kargs['dironly']: options+=4
		if 'noempty' in kargs and kargs['noempty']: options+=8
		if 'statonly' in kargs and kargs['statonly']: options+=16
		if 'inactive' in kargs and kargs['inactive']: options+=32
		if 'unsort' in kargs and kargs['unsort']: options+=64
		if 'nfollowlink' in kargs and kargs['nfollowlink']: options+=128
		#if kargs['condensed']: options+=256
		#if kargs['numbers']: options+=512
		#if kargs['xmlheaders']: options+=1024
		#if kargs['fullname']: options+=2048
		#if kargs['fullugid']: options+=4096
		#if kargs['hier']: options+=8192
		#if kargs['nocase']: options+=16384
		#if kargs['nospanparent']: options+=32768
		#if kargs['all']: options+=131072
		keySet=KeySet(libpyelektra.ksNew())
		libpyelektra.kdbGetKeyChildKeys(self.__kdb__pointer__,key,keySet.__keyset__pointer__,options)
		return keySet
	def getChildKeys(self, name, *args, **kargs):
		options=0
		if 'norecursive' in kargs and kargs['norecursive']: options+=1
		if 'excldir' in kargs and kargs['excldir']: options+=2
		if 'dironly' in kargs and kargs['dironly']: options+=4
		if 'noempty' in kargs and kargs['noempty']: options+=8
		if 'statonly' in kargs and kargs['statonly']: options+=16
		if 'inactive' in kargs and kargs['inactive']: options+=32
		if 'unsort' in kargs and kargs['unsort']: options+=64
		if 'nfollowlink' in kargs and kargs['nfollowlink']: options+=128
		#if kargs['condensed']: options+=256
		#if kargs['numbers']: options+=512
		#if kargs['xmlheaders']: options+=1024
		#if kargs['fullname']: options+=2048
		#if kargs['fullugid']: options+=4096
		#if kargs['hier']: options+=8192
		#if kargs['nocase']: options+=16384
		#if kargs['nospanparent']: options+=32768
		#if kargs['all']: options+=131072
		keySet=KeySet(libpyelektra.ksNew())
		libpyelektra.kdbGetChildKeys(self.__kdb__pointer__,name,keySet.__keyset__pointer__,options)
		return keySet
	def setKey(self,key):
		"""Sets key in the backend storage."""
		return libpyelektra.kdbSetKey(self.__kdb__pointer__,key.__key__pointer__)
	def setKeys(self,keyset):
		"""Commits the ks KeySet to the backend storage, starting from ks's current position until its end.
This is why it is suggested that you call rewind() on ks before calling this method.
Each key is checked with keyNeedSync() before being actually commited. So only changed
keys are updated.  If some error occurs, SetKeys() will stop. In this situation the KeySet 
internal cursor is left on the key that generated the error.
"""
		return libpyelektra.setKey(self.__kdb__pointer__,keyset.__keyset__pointer__)
	def getRootKeys(self):
		"""Returns a KeySet with all root keys currently recognized and present on the system.
Currently, the system and current user's user keys are returned.
This is the only valid way to get to know of the root keys.
"""
		keyset=ksNew()
		libpyelektra.getRootKeys(self.__keyset_pointer__,self.__keyset__pointer__)
		return keyset
	def error(self,no):
		"""Provides an error string associated with errnum"""
		return libpyelektra.kdbStrError(no)
	def errno(self):
		"""return the errno of the last error"""
		return libpyelektra.kdbGetErrno(self.__kdb__pointer__)
	def remove(self,name):
		"""remove the key with the name <name>"""
		return libpyelektra.kdbRemove(self.__kdb__pointer__,name)

class Key:
	"""Is a representation of a key, that can be modified by the following methods"""
	def __init__(self,*args,**kargs):
		#if len(args)==1:
		#	self.__key__pointer__=args[0]
		if kargs.has_key('name'):
			self.__key__pointer__=libpyelektra.keyNew(kargs['name'])
			del kargs['name']
		else:
			self.__key__pointer__=libpyelektra.keyNew('')
		self.set(**kargs)
		if kargs.has_key('value'):
			libpyelektra.keySetString(self.__key__pointer__,kargs['value'])
		if kargs.has_key('lookup'):
			try:
				libpyelektra.kdbGetKey(kargs['lookup'].__kdb__pointer__,self.__key__pointer__)
			except AttributeError:
				raise ElektraException('lookup needs a initialised kdb object')
	#def new(self,name):
	#	"""creates new key"""
	#	self.__key__pointer__=libpyelektra.keyNew(name)
	def set(self, **kargs):
		if kargs.has_key('name'):
			self.setName(kargs['name'])
		if kargs.has_key('value'):
			libpyelektra.keySetString(self.__key__pointer__,kargs['value'])
		if kargs.has_key('lookup'):
			try:
				libpyelektra.kdbGetKey(kargs['lookup'].__kdb__pointer__,self.__key__pointer__)
			except AttributeError:
				raise ElektraException('lookup needs a initialised kdb object')
		if kargs.has_key('set'):
			try:
				libpyelektra.kdbSetKey(kargs['set'].__kdb__pointer__,self.__key__pointer__)
			except AttributeError:
				raise ElektraException('lookup needs a initialised kdb object')
		return self

	def setName(self,name,**kargs):
		"""give the key a new name"""
		ret= libpyelektra.keySetName(self.__key__pointer__,name) 
		if ret == -1:
			raise ElektraException('Keyname invalid')

		if kargs.has_key('lookup'):
			try:
				libpyelektra.kdbGetKey(kargs['lookup'].__kdb__pointer__,self.__key__pointer__)
			except AttributeError:
				raise ElektraException('lookup needs a initialised kdb object')
	def setBinary(self):
		"""Set the binary value of a key"""
		libpyelektra.keySetBinary(self.__key__pointer__,value)
	def getFullName(self):
		"""Set the full name of a key"""
		return libpyelektra.keyGetFullName(self.__key__pointer__)
	def getString(self):
		"""Get the string value of a key"""
		return libpyelektra.keyGetString(self.__key__pointer__)
	def setString(self,s):
		"""Set the full name of a key"""
		return libpyelektra.keySetString(self.__key__pointer__,s)
	def getNameSize(self):
		"""returns the name size"""
		return libpyelektra.keyGetNameSize(self.__key__pointer__)
	def getDataSize(self):
		"""returns the data size"""
		return libpyelektra.keyGetDataSize(self.__key__pointer__)
	def getCommentSize(self):
		"""returns the comment size"""
		return libpyelektra.keyGetCommentSize(self.__key__pointer__)
	def getComment(self):
		"""returns the comment"""
		return libpyelektra.keyGetComment(self.__key__pointer__)
	def getName(self):
		"""returns the key name"""
		return libpyelektra.keyGetName(self.__key__pointer__)
	def dup(self):
		"""returns the duplicate"""
		return Key(libpyelektra.keyDup(self.__key__pointer__))
	def getType(self):
		"""return type of the key"""
		return libpyelektra.keyGetType(self.__key__pointer__)
	def setType(self,type):
		"""set the key type"""
		libpyelektra.keySetType(self.__key__pointer__,type)
	def getRecordSize(self,type):
		"""get the record size of the key"""
		libpyelektra.keyGetRecordSize(self.__key__pointer__)
	def getFullNameSize(self):
		"""get the fullname size"""
		return libpyelektra.keyGetFullNameSize(self.__key__pointer__)
	def name(self):
		"""returns the name of the key"""
		return libpyelektra.keyName(self.__key__pointer__)
	def comment(self):
		"""returns the comment of the key"""
		return libpyelektra.keyComment(self.__key__pointer__)
	def setComment(self,s):
		"""set the key comment"""
		libpyelektra.keySetComment(self.__key__pointer__,s)
	def getUID(self):
		"""return the uid"""
		return libpyelektra.keyGetUID(self.__key__pointer__)
	def setUID(self,uid):
		"""set the uid of the key"""
		return libpyelektra.keySetUID(self.__key__pointer__,uid)
	def getGID(self):
		"""get the GID of the key"""
		return libpyelektra.getGID(self.__key__pointer__)
	def setGID(self,gid):
		"""set the GID of the key"""
		return libpyelektra.setGID(self.__key__pointer__,gid)
	def getMode(self):
		"""get the mode of the key"""
		return libpyelektra.keyGetMode(self.__key__pointer__)
	def setMode(self,a):
		"""set the mode of the key"""
		return libpyelektra.keySetMode(self.__key__pointer__,a)
	def getOwnerSize(self):
		"""returns the owner size"""
		return libpyelektra.keyGetOwnerSize(self.__key__pointer__)
	def owner(self):
		"""returns the owner"""
		return libpyelektra.keyOwner(self.__key__pointer__)
	def setOwner(self):
		"""sets a new owner"""
		return libpyelektra.keySetOwner(self.__key__pointer__)
	def getLink(self):
		"""returns the link"""
		return libpyelektra.keyGetLink(self.__key__pointer__)
	def setLink(self,value):
		"""sets the link"""
		return libpyelektra.keySetLink(self.__key__pointer__)
	def getMTime(self):
		"""returns the MTime"""
		return libpyelektra.keyGetMTime(self.__key__pointer__)
	def getATime(self):
		"""returns the ATime"""
		return libpyelektra.keyGetATime(self.__key__pointer__)
	def getCTime(self):
		"""returns the CTime"""
		return libpyelektra.keyGetCTime(self.__key__pointer__)
	def isSystem(self):
		"""check whether the key is under the system namespace or not"""
		if libpyelektra.keyIsSystem(self.__key__pointer__)==0: return False
		return True
	def isUser(self):
		"""check whether the key is under the user namespace or not"""
		if libpyelektra.keyIsUser(self.__key__pointer__)==0: return False
		return True
	def isDir(self):
		"""check if the key is a folder key or not"""
		if libpyelektra.keyIsDir(self.__key__pointer__)==0: return False
		return True
	def isLink(self):
		"""check if the key is a Link key or not"""
		if libpyelektra.keyIsLink(self.__key__pointer__)==0: return False
		return True
	def isBin(self):
		"""check if the key is a binary type"""
		if libpyelektra.keyIsBin(self.__key__pointer__)==0: return False
		return True
	def isString(self):
		"""check if the key is a string type"""
		if libpyelektra.keyIsBin(self.__key__pointer__)==0: return False
		return True
	#def close(self):
	#	libpyelektra.keyClose(self.__key__pointer__)
	def __del__(self):
		libpyelektra.keyClose(self.__key__pointer__)

class KeySet:
	"""Methods to manipulate KeySets. A KeySet is a linked list to group a number of Keys. 
KeySets have an internal cursor  to help in the Key navigation."""
	def __init__(self,*args):
		if len(args)==1:
			self.__keyset__pointer__=args[0]
	def new(self):
		"""new() creates a new keyset"""
		self.__keyset__pointer__=libpyelektra.ksNew()
	def del_(self):
		"""del_() deletes the keyset"""
		libpyelektra.ksDel(self.__keyset__pointer__)
	def rewind(self):
		"""resets the cursor"""
		libpyelektra.ksRewind(self.__keyset__pointer__)
	def next(self):
		"""returns the next keyset in the keyset"""
		libpyelektra.ksNext(self.__keyset__pointer__)
	def getSize(self):
		"""returns the number of elements in the keyset"""
		libpyelektra.ksGetSize(self.__keyset__pointer__)
	def insert(self,key):
		"""inserts a new Key in the beginning of the KeySet."""
		return libpyelektra.ksInsert(self.__keyset__pointer__,key.__key__pointer__)
	def append(self,key):
		"""appends a new Key at the end of the KeySet."""
		return libpyelektra.ksInsert(self.__keyset__pointer__,key.__key__pointer__)
	def pop(self):
		"""Remove and return the last key of ks"""
		return libpyelektra.ksPop(self.__keyset__pointer__)
	def insertKeys(self,toInsert):
		"""ks.insertKeys(toInsert) transfers all keys from toInsert to the begining of ks.
After this call, toInsert will be empty and can be deleted with toInsert.del()
Returns: the size of the KeySet after transfer."""
		return libpyelektra.ksInsertKeys(self.__keyset__pointer__,toInsert.__keyset__pointer__)
	def appendKeys(self,toAppend):
		"""ks.insertKeys(toAppend) transfers all keys from toInsert to the begining of ks.
After this call, toInsert will be empty and can be deleted with toInsert.del()
Returns: the size of the KeySet after transfer."""
		return libpyelektra.ksInsertKeys(self.__keyset__pointer__,toAppend.__keyset__pointer__)
	def sort(self):
		"""sort the keys in the keyset"""
		libpyelektra.ksSort(self.__keyset__pointer__)
	def lookupByString(self,value,*args,**kargs):
		"""Look for a Key contained in ks that matches name, starting from ks' ksNext() position.
Options: nocase\tignore case"""
		options=0
		if 'nocase' in kargs and kargs['nocase']: options+=32768
		t=libpyelektra.ksLookupByString(self.__keyset__pointer__,value,options)
		if t==0: return None
		return Key(t)

	def lookupByName(self,value,*args,**kargs):
		"""Look for a Key contained in ks that matches name, starting from ks' ksNext() position.
Options: nocase\tignore case"""
		options=0
		if 'nocase' in kargs and kargs['nocase']: options+=32768
		if 'all' in kargs and kargs['all']: options+=131072
		t=libpyelektra.ksLookupByName(self.__keyset__pointer__,value,options)
		if t==0: return None
		return Key(t)

	def lookupByBinary(self,value,options):
		"""Look for a Key contained in ks that matches name, starting from ks' ksNext() position.
Options: nocase\tignore case"""
		options=0
		if 'nocase' in kargs and kargs['nocase']: options+=32768
		t=libpyelektra.ksLookupByBinary(self.__keyset__pointer__,value,options)
		if t==0: return None
		return Key(t)
	def current(self):
		"""returns the current Key or None if you reached the end"""
		t=libpyelektra.ksCurrent(self.__keyset__pointer__)
		if t==0: return None
		return Key(t)
	def head(self):
		"""returns the first key in the KeySet, without changing the KeySet's internal cursor."""
		return Key(libpyelektra.ksHead(self.__keyset__pointer__))
	def tail(self):
		"""returns the last key in the KeySet, without changing the KeySet's internal cursor."""
		return Key(libpyelektra.ksTail(self.__keyset__pointer__))
