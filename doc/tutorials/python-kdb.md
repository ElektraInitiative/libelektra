# How-To: Python kdb

## Introduction

When programming in python it is possible to access the kdb database, changing values of existing keys, adding and deleting keys and a few other things.

## First Steps

In order to being able to use kdb, obviously, you at first need to `import kdb`. You need access to an python-object of kdb. This is accomplished by calling `kdb.KDB()` and saving this to a variable because later on this object will be needed for various operations. 
The easiest way to do this would be:
```
import kdb
with kdb.KDB() as k:
	print 'Hello world! I have a kdb-instance! :D'
	# do all kinds of operations explained below
```

## Keyset

A keyset is basically a list of the keys that lie within the specified range of the databse. When creating an empty keyset this range is obviously zero. It is possible to load the whole database into a keyset but in a lot of cases this is not needed and you can specify which keys exactly you need (which I mean with specified range). At first it is necessary to create a new keyset. When simply calling `kdb.KeySet()` the keyset is of size 0. There is no restriction to the keyset's size. It is possible to specify a certain (maximum) size for a keyset. To load keys into to keyset from the database you simply call the method `get` provided by the kdb-object.

```
import kdb
with kdb.KDB() as k:
	# create empty keyset
	ks = kdb.KeySet()
	print len(ks) # should be 0
	# load an existing keyset
	k.get(ks, '/path/to/keys')
```

It is also possible to iterate as expected over a keyset and use len, reversed and copying. The elements of a keyset can be accessed by indexes and a keyset can be sliced. Another way of accessing a key is by the key-name (`keyset_name['/path/to/keys/key_name']`). If the key-name does not exist within the keyset, a KeyError exception is thrown.

An example that shows how to load an existing keyset and then access every key and value of the loaded keyset:

```
import kdb
with kdb.KDB() as k:
	ks = kdb.KeySet()
	#loading an existing keyset
	k.get(ks, '/path/to/keys')
	# if you for some reason want to loop through the keyset last key first use: for i in reversed(ks):
	for i in ks: 
		# print for every key in the keyset the key and the value
		print 'key: '
		print i
		print 'value: '
		print ks[i]
	
```

Here an example of how you can easily check if a key exists:

```
import kdb
with kdb.KDB() as k:
	ks = kdb.KeySet()
	k.get(ks, '/path/to/keys')
	try:
                print 'The value to the key /user/sw/pk/key_name is ' + ks["/user/sw/pk/key_name"] + '!'
        except KeyError:
                print 'The key does not exist!'
```

Ways of copying a keyset:

```
import kdb
import copy
with kdb.KDB() as k:
	ks = kdb.KeySet()
	k.get(ks, '/path/to/keys')
	# creating a deep copy
	ks_deepcopy = copy.deepcopy(ks)
	# creating a shallow copy
	ks_shallowcopy = copy.copy(ks)
	
```

Slicing works just like for normal lists in python:

```
import kdb
with kdb.KDB() as k:
	ks = kdb.KeySet()
	k.get(ks, '/path/to/keys')
	# creating a shallow copy by slicing
	ks_copy_by_slicing = ks[:]
	# in the following examples start & end need to be replaced by integers
	# create keyset with keys from start to end-1
	a = ks[start:end]
	# create keyset with keys from start to the end of the original keyset
	b = ks[start:]
	# create keyset with keys from the beginning of the keyset to end
	c = ks[:end]
	
```

If you have changed anything in the keyset and want those changes to be saved to the database, you need to call `set` which is just like `get` provided by the kdb-object.

An example of everything up until now could look like this:
```
import kdb
with kdb.KDB() as k:
	ks = kdb.KeySet()
	k.get(ks, '/path/to/keys')
	# ... any number of operations that manipulate the keyset as explained below
	# setting and by doing so saving the new keyset
	k.set(ks, '/path/to/keys') 
	# by changing the path here it is for instance possible to set the keyset of one user identical to another users
```

## Operations with keys

Finding out if any given key exists within a keyset is easy as you simply need to try an access it. To add a new key to a keyset you simply call `append` for for the right keyset. With `ks.lookup("path/to/keys/key_name").string` it is possible to print the current value of the specified key.

```
try:
	key = ks['/path/to/keys/key_name']
except KeyError:
	ks.append(kdb.Key("/path/to/keys/key_name", kdb.KEY_VALUE, "key_value"))
```

