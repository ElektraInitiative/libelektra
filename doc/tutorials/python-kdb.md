# How-To: Python kdb

## Introduction

When programming in python it is possible to access the kdb database, changing values of existing keys, adding and deleting keys and a few other things.

## First Steps

In order to being able to use kdb, obviously, you at first need to `import kdb`. You need access to an python-object of kdb. This is accomlished by calling `kdb.KDB()` and saving this to a variable because later on this object will be needed for various operations.

## Keyset

A keyset is basically a list of the keys that lie within the specified range. At first it is necessairy to create a new keyset. When simply calling `kdb.KeySet()` the keyset is of undefined size. It is possible to specify a certain (maximum) size for a keyset. To load keys into to keyset from the database you simply call the method `get` provided by the kdb-object.

It is also possible to iterate completely normally over a keyset and use len and reversed, dup and deepcopy. The elements of a keyset can be accessed by indexes and a keyset can be sliced. Another way of accessing a key is by the key-name (`keyset_name['path/to/keys/key_name']`). If the key-name does not exist within the keyset, a KeyError exception is thrown.

If you have changed anything in the keyset and want those changes to be saved to the database, you need to call `set` which is just like `get` provided by the kdb-object.

An example of everything up until now could look like this:
```
import kdb
with kdb.KDB() as k:
	ks = kdb.KeySet()
	k.get(ks, 'path/to/keys')
	... # any number of operations that manipulate the keyset
	k.set(ks, 'path/to/keys') # by changing the path here it is for instance possible to set the keyset of one user identical to another users
```

## Operations with keys

Finding out if any given key exists within a keyset is easy as you simply need to try an access it. To add a new key to a keyset you simply call `append` for for the right keyset. With `ks.lookup("path/to/keys/key_name").string` it is possible to print the current value of the specified key.

```
try:
	key = ks['path/to/keys/key_name']
except KeyError:
	ks.append(kdb.Key("path/to/keys/key_name", kdb.KEY_VALUE, "key_value"))
```

