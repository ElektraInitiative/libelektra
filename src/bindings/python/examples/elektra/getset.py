#!/usr/bin/python

# try: 
# > python hello.py

import elektra
import sys

kdb=elektra.Kdb()
kdb.open()

k=elektra.Key()
k.new("user/key")
k.setName("/key")
k.setString("hi")

kdb.setKey(k)

ks=kdb.getChildKeys("user",norecursive=True)

key=ks.lookupByName("/key")
print key.getString()

ks.del_()
kdb.close()
