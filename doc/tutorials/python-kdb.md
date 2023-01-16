# How-To: Python kdb

## Table of Contents

- [Introduction](#Introduction)
- [Installation](#Installation)
  - [Alpine Linux](#Alpine-Linux)
  - [Debian](#Debian)
- [Import kdb](#Import-kdb)
- [Keyset](#Keyset)
- [Keys](#Keys)
- [Merging KeySets](#Merging KeySets)

## Introduction

When programming in Python it is possible to access the kdb database, changing values of existing keys, adding and deleting keys and a few other things.

## Installation

Either [build](https://www.libelektra.org/bindings/swig_python) the package or install from a repository.

### Alpine Linux

The [python bindings package](https://pkgs.alpinelinux.org/packages?name=py3-elektra&branch=edge&repo=testing) is only available in the testing repository (as of 2019-04-29).

```sh
docker run -it alpine:edge /bin/sh
echo "https://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
# Install elektra and the python bindings
apk update && apk add elektra elektra-python py3-elektra
```

Under regular alpine, you have to install python3 from the edge repository. If you do not want to add the edge repositories permanently like above, you can do

```sh
apk add --repository "https://dl-cdn.alpinelinux.org/alpine/edge/main" python3
apk add --repository "https://dl-cdn.alpinelinux.org/alpine/edge/testing" elektra elektra-python py3-elektra
```

### Debian

The Elektra python-binding is currently built for:

- Debian 11 `bullseye`
- Debian 10 `buster`

```sh
docker run -it debian:bullseye
apt-get update
apt-get install ca-certificates
apt-get install vim gnupg
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F26BBE02F3C315A19BF1F791A9A25CC1CC83E839
vim /etc/apt/sources.list
```

Append `deb https://debs.libelektra.org/bullseye bullseye main` to `/etc/apt/sources.list` and install it:

```sh
apt-get update
apt-get install python3-elektra
```

## Import kdb

In order to being able to use `kdb`, you at first need to `import kdb`. You need access to a Python object of `KDB`. This is accomplished by calling `kdb.KDB()` and saving this to a variable because later on this object will be needed for various operations.
The easiest way to do this would be:

```py
import kdb

with kdb.KDB() as k:
    print("Hello world! I have a kdb-instance! :D")
    # do all kinds of operations explained below
```

## Keyset

A keyset is basically a list of the keys that lie within the specified range of the database. When creating an empty keyset this range is obviously zero. It is possible to load the whole database into a keyset but in a lot of cases this is not needed and you can specify which keys exactly you need (which I mean with specified range). At first it is necessary to create a new keyset. When simply calling `kdb.KeySet()` the keyset is of size 0. There is no restriction to the keyset's size. It is possible to specify a certain (maximum) size for a keyset. To load keys into to keyset from the database you simply call the method `get` provided by the kdb-object.

```py
import kdb

with kdb.KDB() as k:
    # create empty keyset
    ks = kdb.KeySet()
    print(len(ks))  # should be 0
    # load an existing keyset
    k.get(ks, 'user')
```

It is also possible to iterate as expected over a keyset and use `len`, `reversed` and copying. The elements of a keyset can be accessed by indexes and a keyset can be sliced. Another way of accessing a key is by the key name (`keyset_name['/path/to/keys/key_name']`). If a key with the given name does not exist within the keyset, a `KeyError` exception is thrown.

An example that shows how to load an existing keyset and then access every key and value of the loaded keyset:

```py
import kdb

with kdb.KDB() as k:
    ks = kdb.KeySet()
    # load an existing keyset
    k.get(ks, 'user')
    # if you for some reason want to loop through the keyset last key first
    # use: for key in reversed(ks):
    for key in ks:
        # print for every key in the keyset the key and the value
        print("key: {} value: {}".format(key, key.value))
```

Here an example of how you can easily check if a key exists:

```py
import kdb

with kdb.KDB() as k:
    namespace = "user:/"
    path = '{}/test'.format(namespace)

    ks = kdb.KeySet()
    k.get(ks, namespace)

    try:
        print("The value of the key {} is {}!".format(ks[path], ks[path]
                                                      .value))
    except KeyError:
        print("The key {} does not exist!".format(path))
```

Ways of copying a keyset:

```py
import copy, kdb

with kdb.KDB() as k:
    ks = kdb.KeySet()
    k.get(ks, 'spec')
    # creating a deep copy
    ks_deepcopy = copy.deepcopy(ks)
    # creating a shallow copy
    ks_shallowcopy = copy.copy(ks)
```

Slicing works just like for normal lists in Python. But be careful: Afterwards the result will be a list - not a keyset.

```py
import kdb

with kdb.KDB() as k:
    ks = kdb.KeySet()
    k.get(ks, 'system')

    # creating a shallow copy by slicing
    ks_copy_by_slicing = ks[:]

    # in the following examples start & end need to be replaced by integers
    start, end = 1, 3
    # create keyset with keys from start to end-1
    a = ks[start:end]

    # create keyset with keys from start to the end of the original keyset
    start = len(ks) - 3
    b = ks[start:]

    # create keyset with keys from the beginning of the keyset to end
    c = ks[:end]

    for keyset in [a, b, c]:
        for key in keyset:
            try:
                print('{}: {}'.format(key, key.value))
            except UnicodeDecodeError:
                pass  # Ignore keys and values that store non-ASCII characters
        print('')
```

If you have changed anything in the keyset and want those changes to be saved to the database, you need to call `set` which is just like `get` provided by the kdb-object.

An example of everything up until now could look like this:

```py
import kdb

with kdb.KDB() as k:
    ks = kdb.KeySet()
    k.get(ks, '/path/to/keys')

    # Any number of operations that manipulate the keyset as explained above

    # Setting and by doing so saving the new keyset
    k.set(ks, '/path/to/keys')
    # by changing the path here it is for instance possible to set the keyset
    # of one user identical to another users
```

If you have a key a very simple way to get its name and value:

```py
import kdb

with kdb.KDB() as k:
    ks = kdb.KeySet()
    k.get(ks, 'user')
    for key in ks:
        print("key name:  {}".format(key.name))
        print("{0}{1}\n{0}{2}\n".format("key value: ", key.value,
                                        ks.lookup(key).string))
```

## Keys

It is possible to create new keys:

```py
import kdb

with kdb.KDB() as k:
    # the first argument is the path to the key,
    # the third argument is the key's value
    new_key = kdb.Key('/user/sw/pk/key_name', kdb.KEY_VALUE, 'key_value')
    print("{}: {}".format(new_key, new_key.value))
```

You can also duplicate a key:

```py
import kdb

with kdb.KDB() as k:
    key1 = kdb.Key('/user/sw/pk/key_name', kdb.KEY_VALUE, 'key_value')
    key2 = kdb.Key(key1.dup())
    print("{}: {}".format(new_key, new_key.value))
    print("{}: {}".format(key2, key2.value))
```

An example for working with meta-keys

```py
import kdb

with kdb.KDB() as k:
    key1 = kdb.Key("user:/key1", kdb.KEY_VALUE, "some_value")
    key1.setMeta("foo",     "bar")
    key1.setMeta("owner",   "manuel")
    key1.setMeta("comment/#0", "this is my example key")
    for meta in key1.getMeta():
        print("  key1.{0} = \"{1}\"".format(meta.name, meta.value))
```

Keys can be added to a keyset using `append`. If the key already exists, the value will be updated. Calling `keyset_name['/path/to/key'] = 'new_value` does not work for updating keys already in a keyset. Keys can be removed with `pop`, `remove` or `cut`

```py
from kdb import KDB, KEY_VALUE, Key, KeySet


class KeySet(KeySet):
    def __repr__(self):
        """Return a textual representation of this keyset."""
        return '\n'.join(
            ['{}: {}'.format(key.name, key.value) for key in self])


def key(name, value):
    """Create a new key with the given name and value."""
    return Key(name, KEY_VALUE, value)


def describe(keyset, title, newline=True):
    return '{}\n{}\n{}{}'.format(title, '=' * len(title), keyset,
                                 '\n' if newline else '')


with KDB() as data:
    keyset = KeySet()
    data.get(keyset, 'user')
    print(describe(keyset, "Initial Keyset"))

    new_key = key('user:/sw/pk/key_name', 'key_value')
    # adding new_key to the existing key-set,
    # ks['/user/sw/pk/key_name'].value == 'key_value'
    keyset.append(new_key)
    print(describe(keyset, "Add New Key"))

    newer_key = key('user:/sw/pk/key_name', 'other_key_value')
    # adding newer_key to the existing key-set, by doing so replacing new_key,
    # ks['/user/sw/pk/key_name'].value == 'other_key_value'
    keyset.append(newer_key)
    print(describe(keyset, "Replace Key", newline=False))
```

## Merging KeySets

The internal three-way merge algorithm is also included in the Python bindings.

```py
import kdb, kdb.merge

baseKeys = kdb.KeySet(100,
                    kdb.Key("system:/test/key1", "k1"),
                    kdb.Key("system:/test/key2", "k2"),
                    kdb.KS_END,
                    )

ourKeys = kdb.KeySet(100,
                     kdb.Key("system:/test/key1", "k1"),
                     kdb.Key("system:/test/key3", "k3"),
                     kdb.KS_END,
                     )

theirKeys = kdb.KeySet(100,
                       kdb.Key("system:/test/key1", "k1"),
                       kdb.Key("system:/test/key4", "k4"),
                       kdb.KS_END,
                       )

base = kdb.merge.MergeKeys(baseKeys, kdb.Key("system:/test"))
theirs = kdb.merge.MergeKeys(theirKeys, kdb.Key("system:/test"))
ours = kdb.merge.MergeKeys(ourKeys, kdb.Key("system:/test"))

merger = kdb.merge.Merger()
mergeResult = merger.merge(base, ours, theirs, kdb.Key("system:/test"), kdb.merge.ConflictStrategy.THEIR)

# prints ['system:/test/key1', 'system:/test/key3', 'system:/test/key4']
print(mergeResult.mergedKeys)
```
