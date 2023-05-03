# How-To: Write a (Well Behaved) Storage Plugin

The [plugin tutorial](plugins.md) already covers some of the most interesting parts on how to write a (storage) plugin. This text will tell you a little bit more about how a storage plugin should act. While it is usually relatively easy to create a plugin that stores basic key-value pairs, adding advanced features such as support for

- [arrays](arrays.md), and
- [metadata](../dev/metadata.md),

takes more work. Before you continue with this text, please make sure that you read all of the linked documents above.

## Don’t Add Additional Keys

One common problem of storage plugins is, that they store too many keys. For example, if the user adds the keys

- `user:/tests/storage/root` and
- `user:/tests/storage/root/level1/level2/level3`,

then your plugin should only store those two keys. **Do not** add the keys

- `user:/tests/storage/root/level1`, or
- `user:/tests/storage/root/level1/level2`

to the key set. One plugin that handles this situation properly is [YAML CPP](/src/plugins/yamlcpp/), as the following [Markdown Shell Recorder][] test shows:

```sh
# Mount plugin
sudo kdb mount config.yaml user:/tests/storage yamlcpp

# Add key-value pairs
kdb set user:/tests/storage/root 🐓
kdb set user:/tests/storage/root/level1/level2/level3 🐣

# Make sure that YAML CPP did not store any additional keys
kdb ls user:/tests/storage/root
#> user:/tests/storage/root
#> user:/tests/storage/root/level1/level2/level3

# Undo modifications to the key database
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

. For more information on why we allow “holes” in the hierarchy, please take a look [here](../decisions/6_implemented/holes.md).

[markdown shell recorder]: https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper

## Differentiate Between Empty Keys and Keys Containing an Empty String

Elektra supports both binary and textual values. The main difference between binary and textual data is that textual data always ends with a null byte. Therefore you are not allowed to store the code point `0` inside textual data. Binary data does not have this limitation.

The simplest textual data is the empty string (`""` = `0`) and has length 1, while the simplest binary data stores nothing at all and therefore has length 0. In the `kdb` utility you can disambiguate between these value by checking for the [metakey `binary`](../help/elektra-metadata.md). The following [Markdown Shell Recorder][] test shows how a storage plugin should handle empty values.

```sh
# Mount plugin
sudo kdb mount config.yaml user:/tests/storage yamlcpp

kdb set user:/tests/storage/empty ''
#> Create a new key user:/tests/storage/empty with string ""
kdb get user:/tests/storage/empty
#>
kdb meta-ls user:/tests/storage/empty
#>

# Undo modifications to the key database
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

## Convert Boolean Data

Elektra uses [`0` and `1` to represent binary data](../decisions/5_partially_implemented/boolean.md).
A storage plugin that uses other values (e.g. `false` and `true`) needs to convert these values to `0` and `1`.
The [Markdown Shell Recorder][] test below shows that [YAML CPP](../../src/plugins/yamlcpp/README.md) handles the conversion from and to [YAML’s boolean type](https://yaml.org/spec/1.2/spec.html#id2803629) properly.
In the test we also use the [`type` plugin](../../src/plugins/type/README.md) to makes sure that YAML CPP interacts correctly with this essential plugin.

```sh
# Mount plugin
sudo kdb mount config.yaml user:/tests/storage yamlcpp type

kdb set user:/tests/storage/bool/value true
kdb meta-set user:/tests/storage/bool/value type boolean
kdb get user:/tests/storage/bool/value
#> 1

kdb set user:/tests/storage/bool/value 1
kdb get user:/tests/storage/bool/value
#> 1

kdb set user:/tests/storage/bool/value false
kdb get user:/tests/storage/bool/value
#> 0

kdb set user:/tests/storage/bool/value 'non boolean'
# RET: 5

kdb get user:/tests/storage/bool/value
#> 0

# Undo modifications to the key database
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

## Support Values Inside Non-Leaf Keys

Sometimes the most “natural” mapping of key-value pairs to a file format might cause a storage plugin to not be able to store values in so-called directory (non-leaf) keys.

For example, in a key set that contains the keys:

```
user:/directory
user:/directory/leaf1
user:/directory/leaf2
user:/leaf3
```

, all keys at the bottom of the hierarchy:

```
        user
      /      \
  directory  leaf3
   /     \
leaf1   leaf2
```

, such as

- `user:/directory/leaf1`
- `user:/directory/leaf2`
- `user:/leaf3`

are called leaf keys, while `user:/directory` is a directory key. Plugins such as [YAJL](/src/plugins/yajl/) or [YAML CPP](/src/plugins/yamlcpp/) will not be able to store data in the key with the name `user:/directory` directly. To work around this issue these plugin use the [Directory Value plugin](/src/plugins/directoryvalue/). In the ReadMe of the [Directory Value plugin](https://www.libelektra.org/plugins/directoryvalue) and [YAML CPP](https://www.libelektra.org/plugins/yamlcpp) you will find more information about this issue, and how to handle it.

The following Markdown Shell Recorder test shows **the proper behavior**:

```sh
# Mount plugin
sudo kdb mount config.yaml user:/tests/storage yamlcpp

# Add key-value pair (leaf key)
kdb set user:/tests/storage/root 🐓
# Since we add a key below `user:/tests/storage/root`, the key
# `user:/tests/storage/root` turns from a leaf key to a directory key.
kdb set user:/tests/storage/root/level1/level2/level3 🐣

# Make sure that the directory key still stores the correct value
kdb get user:/tests/storage/root
#> 🐓

# Check the value of the leaf key
kdb get user:/tests/storage/root/level1/level2/level3
#> 🐣

# Undo modifications to the key database
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

. To make sure that your storage plugin works correctly, please just replace `yamlcpp` with the name of your plugin and verify that the test above still works.

## Support Array And Non-Array Data Properly

You already learned about the array syntax and the mandatory `array` metakey in the [array tutorial](arrays.md). Now it is time to check, if your storage plugin supports array and non-array keys properly. Let us look at a concrete example. We use a key set that contains the following keys as example:

```
user:/tests/storage/array
user:/tests/storage/array/#0
user:/tests/storage/array/#1
user:/tests/storage/map
user:/tests/storage/map/#0
user:/tests/storage/map/#1
```

. If we assume that only `user:/tests/storage/array` stores the metakey `array`, then the keys

- `user:/tests/storage/array/#0`, and
- `user:/tests/storage/array/#1`

represent array elements, while

- `user:/tests/storage/map/#0`, and
- `user:/tests/storage/map/#1`

are normal key-value pairs. The following example shows that the storage plugin [YAML CPP](https://www.libelektra.org/plugins/yamlcpp) handles this situation properly:

```sh
# Mount plugin
sudo kdb mount config.yaml user:/tests/storage yamlcpp

# Create an array containing two elements
kdb meta-set user:/tests/storage/array array ''
kdb set user:/tests/storage/array/#0 one
kdb set user:/tests/storage/array/#1 two

# The array parent key stores the basename of the last element
kdb meta-get user:/tests/storage/array array
#> #1

# If you do not add the metakey `array`, then keys
# containing array syntax `#0`, `#1`, … will not be
# interpreted as arrays.
kdb set user:/tests/storage/map ""
kdb set user:/tests/storage/map/#0 ""
kdb set user:/tests/storage/map/#1 ""
kdb meta-get user:/tests/storage/map array
# RET: 12

# Undo modifications to the key database
kdb rm -r user:/tests/storage
sudo kdb umount user:/tests/storage
```

## Storing Comments

Most markup languages provide the possibility of adding comments.
Elektra can store those comments in its metadata as well.
This can be achieved by setting the meta Keys `comment/#` for
the respective configuration Key. Also the `hosts` plugin stores
the comments of the file in the respective Elektra configuration Key:

```sh
# Mount empty hosts file
sudo kdb mount --with-recommends hosts user:/tests/hosts hosts

# Add a line to the hosts file containing a comment
echo '127.0.0.1    localhost # test comment' >  `kdb file user:/tests/hosts`

# Check if the line has been synced successfully
kdb get user:/tests/hosts/ipv4/localhost
#> 127.0.0.1

kdb meta-ls user:/tests/hosts/ipv4/localhost
#> comment/#0
#> comment/#0/space
#> comment/#0/start
#> order

kdb meta-get user:/tests/hosts/ipv4/localhost 'comment/#0'
#>  test comment

# Undo modifications to the key database
kdb rm -r user:/tests/hosts
sudo kdb umount user:/tests/hosts
```

## Ordering of Elements

If your plugin also has the ability to store configuration options in a certain order, then this is also supported by Elektra. Keys can have the metakey `order`, which indicates in which order lines should be written back to the configuration file. Inversely, when reading from configuration files, plugins should add the `order` metakey to the respective KDB entries.

This behavior can be illustrated via the usage of the `hosts` plugin, which honors this convention:

```sh
# Mount empty hosts file
sudo kdb mount --with-recommends hosts user:/tests/hosts hosts

# Add lines to the hosts file
echo '127.0.0.1    localhost.1' >  `kdb file user:/tests/hosts`
echo '127.0.0.1    localhost.2' >>  `kdb file user:/tests/hosts`

# Check if the lines have been synced successfully
kdb ls user:/tests/hosts/ipv4
#> user:/tests/hosts/ipv4/localhost.1
#> user:/tests/hosts/ipv4/localhost.2

# Checking the created Meta KeySet
kdb meta-ls user:/tests/hosts/ipv4/localhost.1
#> comment/#0
#> order

# Getting the content of the order
kdb meta-get user:/tests/hosts/ipv4/localhost.1 order
#> 1

kdb meta-get user:/tests/hosts/ipv4/localhost.2 order
#> 2

# adding some additional Keys out of order
kdb set user:/tests/hosts/ipv4/localhost.4 127.0.0.1
kdb set user:/tests/hosts/ipv4/localhost.3 127.0.0.1

# lines in hosts file have improper ordering
cat `kdb file user:/tests/hosts`
#> 127.0.0.1	localhost.3
#> 127.0.0.1	localhost.4
#> 127.0.0.1	localhost.1
#> 127.0.0.1	localhost.2

# setting the correct order
kdb meta-set user:/tests/hosts/ipv4/localhost.4 order 4
kdb meta-set user:/tests/hosts/ipv4/localhost.3 order 3

# lines in hosts file are also in correct order afterwards
cat `kdb file user:/tests/hosts`
#> 127.0.0.1	localhost.1
#> 127.0.0.1	localhost.2
#> 127.0.0.1	localhost.3
#> 127.0.0.1	localhost.4

# Undo modifications to the key database
kdb rm -r user:/tests/hosts
sudo kdb umount user:/tests/hosts
```

As you can see by setting the order metakey in the respective KDB entries, we can manipulate the order in which entries get written to the hosts file. Also when importing from the initial hosts file, the plugin stores the correct order in the meta KeySet.

<!--
TODO: Add section about relative keys (See also: https://issues.libelektra.org/51)
-->
