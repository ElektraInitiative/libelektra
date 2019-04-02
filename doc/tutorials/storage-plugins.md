# How-To: Write a (Well Behaved) Storage Plugin

The [plugin tutorial](plugins.md) already covers some of the most interesting parts on how to write a (storage) plugin. This text will tell you a little bit more about how a storage plugin should act. While it is usually relatively easy to create a plugin that stores basic key-value pairs, adding advanced features such as support for

- [arrays](arrays.md), and
- [metadata](../dev/metadata.md),

takes more work. Before you continue with this text, please make sure that you read all of the linked documents above.

## Don’t Add Additional Keys

One common problem of storage plugins is, that they store too many keys. For example, if the user adds the keys

- `user/tests/storage/root` and
- `user/tests/storage/root/level1/level2/level3`,

then your plugin should only store those two keys. **Do not** add the keys

- `user/tests/storage/root/level1`, or
- `user/tests/storage/root/level1/level2`

to the key set. One plugin that handles this situation properly is [YAML CPP](/src/plugins/yamlcpp/), as the following [Markdown Shell Recorder test](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper) shows:

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/storage yamlcpp

# Add key-value pairs
kdb set user/tests/storage/root 🐓
kdb set user/tests/storage/root/level1/level2/level3 🐣

# Make sure that YAML CPP did not store any additional keys
kdb ls user/tests/storage/root
#> user/tests/storage/root
#> user/tests/storage/root/level1/level2/level3

# Undo modifications to the key database
kdb rm -r user/tests/storage
sudo kdb umount user/tests/storage
```

. For more information on why we allow “holes” in the hierarchy, please take a look [here](../decisions/holes.md).

## Support Values Inside Non-Leaf Keys

Sometimes the most “natural” mapping of key-value pairs to a file format might cause a storage plugin to not be able to store values in so called directory (non-leaf) keys.

For example, in a key set that contains the keys:

```
user/directory
user/directory/leaf1
user/directory/leaf2
user/leaf3
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

- `user/directory/leaf1`
- `user/directory/leaf2`
- `user/leaf3`

are called leaf keys, while `user/directory` is a directory key. Plugins such as [YAJL](/src/plugins/yajl/) or [YAML CPP](/src/plugins/yamlcpp/) will not be able to store data in the key with the name `user/directory` directly. To work around this issue these plugin use the [Directory Value plugin](/src/plugins/directoryvalue/). In the ReadMe of the [Directory Value plugin](https://www.libelektra.org/plugins/directoryvalue) and [YAML CPP](https://www.libelektra.org/plugins/yamlcpp) you will find more information about this issue, and how to handle it.

The following Markdown Shell Recorder test shows **the proper behavior**:

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/storage yamlcpp

# Add key-value pair (leaf key)
kdb set user/tests/storage/root 🐓
# Since we add a key below `user/tests/storage/root`, the key
# `user/tests/storage/root` turns from a leaf key to a directory key.
kdb set user/tests/storage/root/level1/level2/level3 🐣

# Make sure that the directory key still stores the correct value
kdb get user/tests/storage/root
#> 🐓

# Check the value of the leaf key
kdb get user/tests/storage/root/level1/level2/level3
#> 🐣

# Undo modifications to the key database
kdb rm -r user/tests/storage
sudo kdb umount user/tests/storage
```

. To make sure that your storage plugin works correctly, please just replace `yamlcpp` with the name of your plugin and verify that the test above still works.

## Support Array And Non-Array Data Properly

You already learned about the array syntax in the [array tutorial](arrays.md). Now it is time to check, if your storage plugin supports array and non-array keys properly. Let us look at an concrete example. In the key set that contains keys with the following names:

```
user/tests/storage/array/#0
user/tests/storage/array/#1
user/tests/storage/map/#0
user/tests/storage/map/key
user/tests/storage/map/#1
```

the keys:

- `user/tests/storage/array/#0`, and
- `user/tests/storage/array/#1`

represent array elements, while

- `user/tests/storage/map/#0`, and
- `user/tests/storage/map/#1`

do not, since the key set also contains the key **`user/tests/storage/map/key`**. The following example shows that the storage plugin [YAML CPP](https://www.libelektra.org/plugins/yamlcpp) handles this situation properly:

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/storage yamlcpp

# Create an array containing two elements
kdb set user/tests/storage/array/#0 one
kdb set user/tests/storage/array/#1 two

# The plugin creates an array parent key
# that stores the basename of the last element
kdb getmeta user/tests/storage/array array
#> #1

# Add an array that contains a single element
kdb set user/tests/storage/map/#0
kdb getmeta user/tests/storage/map array
#> #0

# After we add `user/tests/storage/map/key`,
# `user/tests/storage/map` is not an array any more.
kdb set user/tests/storage/map/key three
kdb getmeta user/tests/storage/map array
# RET: 1

# Adding a another key that uses array syntax below
# `user/tests/storage/map` does not change this.
kdb set user/tests/storage/map/#1 four
kdb getmeta user/tests/storage/map array
# RET: 1

# If we remove the key `user/tests/storage/map/key`, then
# `user/tests/storage/map` represents an array again.
kdb rm user/tests/storage/map/key
kdb ls user/tests/storage/map
#> user/tests/storage/map
#> user/tests/storage/map/#0
#> user/tests/storage/map/#1
kdb getmeta user/tests/storage/map array
#> #1

# Undo modifications to the key database
kdb rm -r user/tests/storage
sudo kdb umount user/tests/storage
```

.

<!--
TODO: Describe difference between keys that store null values (binary data) and empty values (string data)
TODO: Add information on how plugins should store comment (meta)data
TODO: Document that a plugin should keep the ordering of key-value pairs of a document intact, when writing data back to the configuration file
TODO: Add section about relative keys (See also: https://issues.libelektra.org/51)
-->
