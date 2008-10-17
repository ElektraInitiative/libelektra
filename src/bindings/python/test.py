#!/usr/bin/python
# try:
#     python test.py -v
# You need the kdb tool installed


"""
>>> ###################### libelektra tests   ########################
>>> # Set user/key using the kdbtool and then remove it using kdbRemove
>>> import libelektra
>>> import elektra
>>> import os
>>> import string
>>> os.system("kdb set user/key 'text'")
0
>>> kdb=libelektra.kdbOpen()
>>> libelektra.kdbRemove(kdb,"user/key")
0
>>> ks=libelektra.ksNew()
>>> string.strip(os.popen("kdb get user/key").read())
''
>>> # Create a new key using libelektra and check the new key using kdb
>>> k=libelektra.keyNew("user/key")
>>> libelektra.keySetString(k,"text")!=0
True
>>> libelektra.kdbSetKey(kdb,k)
0
>>> string.strip(os.popen("kdb get user/key").read())
'text'
>>> libelektra.kdbGetChildKeys(kdb,"user",ks,1)!=-1
True
>>> k=libelektra.ksLookupByName(ks,"/key",0)
>>> libelektra.keyGetString(k)
'text'
>>> # Close the handles
>>> libelektra.keyClose(k)
0
>>> libelektra.kdbClose(kdb)
0
>>> #######################  elektra tests  #######################
>>> kdb=elektra.Kdb()
>>> kdb.open()
>>> os.system("kdb set user/key 'text'")
0
>>> # Remove key
>>> kdb.remove("user/key")
0
>>> # Set Key
>>> ks=elektra.KeySet()
>>> ks.new()
>>> k=elektra.Key()
>>> k.new("user/key")
>>> k.setString("text")
5
>>> kdb.setKey(k)
0
>>> string.strip(os.popen("kdb get user/key").read())
'text'
>>> ks=kdb.getChildKeys("user",norecursive=True)
>>> key=ks.lookupByName("/key")
>>> key.getString()
'text'
>>> k.close()
>>> kdb.close()
"""

def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    _test()
