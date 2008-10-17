#!/usr/bin/python

# try: 
# > kdb set user/hello "Hello World!"
# > python hello.py

import elektra
import sys

#ks=elektra.KeySet()

kdb=elektra.Kdb()
kdb.open()
ks=kdb.getChildKeys("user",norecursive=True)

key=ks.lookupByName("/hello",0)
print key.getString()

ks.del_()
kdb.close()
