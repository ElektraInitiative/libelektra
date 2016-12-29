# How-To: Python kdb

## Introduction

When programming in python it is possible to access the kdb database, changing values of existing keys, adding and deleting keys and a few other things.

## First Steps

In order to being able to use kdb, obviously, you at first need to `import kdb`. You need access to an python-object of kdb. This is accomplished by calling `kdb.KDB()` and saving this to a variable because later on this object will be needed for various operations.
The easiest way to do this would be:
```
import kdb
with kdb.KDB() as k:
	print('Hello world! I have a kdb-instance! :D')
	# do all kinds of operations explained below
```

## Keyset

A keyset is basically a list of the keys that lie within the specified range of the database. When creating an empty keyset this range is obviously zero. It is possible to load the whole database into a keyset but in a lot of cases this is not needed and you can specify which keys exactly you need (which I mean with specified range). At first it is necessary to create a new keyset. When simply calling `kdb.KeySet()` the keyset is of size 0. There is no restriction to the keyset's size. It is possible to specify a certain (maximum) size for a keyset. To load keys into to keyset from the database you simply call the method `get` provided by the kdb-object.

```
import kdb
with kdb.KDB() as k:
	# create empty keyset
	ks = kdb.KeySet()
	print(len(ks)) # should be 0
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
	# if you for some reason want to loop through the keyset last key first use:
	# for i in reversed(ks):
	for i in ks:
		# print for every key in the keyset the key and the value
		print('key: '+ str(i) + 'value: ' + str(ks[i]))
```

Here an example of how you can easily check if a key exists:

```
import kdb
with kdb.KDB() as k:
	ks = kdb.KeySet()
	k.get(ks, '/path/to/keys')
	try:
		print('The value to the key /user/sw/pk/key_name is ' +
		      str(ks['/user/sw/pk/key_name']) + '!')
        except KeyError:
                print('The key does not exist!')
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

Slicing works just like for normal lists in python. But be careful: Afterwards the result will be a list - not a keyset.

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
	# by changing the path here it is for instance possible to set the keyset of
	# one user identical to another users
```

If you have a key a very simple way to get its name and value:

```
import kdb
with kdb.KDB() as k:
	ks = kdb.KeySet()
	k.get(ks, '/path/to/keys')
	for i in ks:
		print('key-name: ' + i.name)
		print('key-value: ' + i.value)
		print('key-value: ' + ks.lookup(i).string)
```

It is possible to create new keys:

```
import kdb
with kdb.KDB() as k:
	# the first argument is the path to the key, the third argument is the key-value
	new_key = kdb.Key('/user/sw/pk/key_name', kdb.KEY_VALUE, 'key_value')
```

Keys can be added to a keyset using `append`. If the key already exists, the value will be updated. Calling `keyset_name['/path/to/key'] = 'new_value` does not work for updating keys already in a keyset.

```
import kdb
with kdb.KDB() as k:
	ks = kdb.KeySet()
	k.get(ks, '/path/to/keys')
	new_key = kdb.Key('/user/sw/pk/key_name', kdb.KEY_VALUE, 'key_value')
	# adding new_key to the existing key-set, ks['/user/sw/pk/key_name'].value == 'key_value'
	ks.append(new_key)
	newer_key = kdb.Key('/user/sw/pk/key_name', kdb.KEY_VALUE, 'other_key_value')
	# adding newer_key to the existing key-set, by doing so replacing new_key,
	# ks['/user/sw/pk/key_name'].value == 'other_key_value'
	ks.append(newer_key)
```



