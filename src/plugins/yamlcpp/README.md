- infos = Information about the yamlcpp plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = base64 directoryvalue
- infos/provides = storage/yaml
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained preview experimental unfinished concept discouraged
- infos/metadata =
- infos/description = This storage plugin reads and writes data in the YAML format

# YAML CPP

## Introduction

The YAML CPP plugin reads and writes configuration data via the [yaml-cpp][] library.

## Usage

You can mount this plugin via `kdb mount`:

```sh
sudo kdb mount config.yaml /examples/yamlcpp yamlcpp
```

. To unmount the plugin use  `kdb umount`:

```sh
sudo kdb umount /examples/yamlcpp
```

. The following examples show how you can store and retrieve data via `yamlcpp`.

```sh
# Mount yamlcpp plugin to cascading namespace `/examples/yamlcpp`
sudo kdb mount config.yaml /examples/yamlcpp yamlcpp

# Manually add a mapping to the database
echo "🔑 : 🐳"               > `kdb file /examples/yamlcpp`
# Retrieve the value of the manually added key
kdb get /examples/yamlcpp/🔑
#> 🐳

# Manually add syntactically incorrect data
echo "some key: @some  value" >> `kdb file /examples/yamlcpp`
kdb get "/examples/yamlcpp/some key"
# STDERR: .*yaml-cpp: error at line 2, column 11: unknown token.*
# ERROR: 10
# RET: 5

# Overwrite incorrect data
echo "🔑: value" >  `kdb file /examples/yamlcpp`

# Add some values via `kdb set`
kdb set /examples/yamlcpp/fleetwood mac
kdb set /examples/yamlcpp/the chain

# Retrieve the new values
kdb get /examples/yamlcpp/the
#> chain
kdb get /examples/yamlcpp/fleetwood
#> mac

# Undo modifications
kdb rm -r /examples/yamlcpp
sudo kdb umount /examples/yamlcpp
```

## Arrays

YAML CPP provides basic support for Elektra’s array data type.

```sh
# Mount yamlcpp plugin to cascading namespace `/examples/yamlcpp`
sudo kdb mount config.yaml /examples/yamlcpp yamlcpp

# Manually add an array to the database
echo 'sunny:'       >  `kdb file /examples/yamlcpp`
echo '  - Charlie'  >> `kdb file /examples/yamlcpp`
echo '  - Dee'      >> `kdb file /examples/yamlcpp`

# List the array entries
kdb ls /examples/yamlcpp
#> user/examples/yamlcpp/sunny
#> user/examples/yamlcpp/sunny/#0
#> user/examples/yamlcpp/sunny/#1

# Read an array entry
kdb get user/examples/yamlcpp/sunny/#1
#> Dee

# You can retrieve the last index of an array by reading the metakey `array`
kdb getmeta /examples/yamlcpp/sunny array
# 1

# Extend the array
kdb set user/examples/yamlcpp/sunny/#2 Dennis
kdb set user/examples/yamlcpp/sunny/#3 Frank
kdb set user/examples/yamlcpp/sunny/#4 Mac

# Retrieve the last array entry
kdb get user/examples/yamlcpp/sunny/$(kdb getmeta user/examples/yamlcpp/sunny array)
#> Mac

# Undo modifications to the key database
kdb rm -r /examples/yamlcpp
sudo kdb umount /examples/yamlcpp
```

The plugin also supports nested arrays.

```sh
# Mount yamlcpp plugin to cascading namespace `/examples/yamlcpp`
sudo kdb mount config.yaml /examples/yamlcpp yamlcpp

# Add some key value pairs
kdb set /examples/yamlcpp/key value
kdb set /examples/yamlcpp/array/#0 scalar
kdb set /examples/yamlcpp/array/#1/key value
kdb set /examples/yamlcpp/array/#1/🔑 🙈

kdb ls /examples/yamlcpp
#> user/examples/yamlcpp/array
#> user/examples/yamlcpp/array/#0
#> user/examples/yamlcpp/array/#1
#> user/examples/yamlcpp/array/#1/key
#> user/examples/yamlcpp/array/#1/🔑
#> user/examples/yamlcpp/key

# Retrieve part of an array value
kdb get /examples/yamlcpp/array/#1/key
#> value

# Since an array saves a list of values, an array parent
# - which represent the array - does not store a value!
echo "/examples/yamlcpp/array: “`kdb get /examples/yamlcpp/array`”"
#> /examples/yamlcpp/array: “”
kdb get /examples/yamlcpp/array/#1
echo "/examples/yamlcpp/array/#1: “`kdb get /examples/yamlcpp/array/#1`”"
#> /examples/yamlcpp/array/#1: “”

# Remove part of an array value
kdb rm /examples/yamlcpp/array/#1/key

kdb ls /examples/yamlcpp
#> user/examples/yamlcpp/array
#> user/examples/yamlcpp/array/#0
#> user/examples/yamlcpp/array/#1
#> user/examples/yamlcpp/array/#1/🔑
#> user/examples/yamlcpp/key

# Undo modifications to the key database
kdb rm -r /examples/yamlcpp
sudo kdb umount /examples/yamlcpp
```

## Metadata

The plugin supports metadata. The example below shows how a basic `Key` including some metadata, looks inside the YAML configuration file:

```yaml
key without metadata: value
key with metadata:
  !elektra/meta
    - value2
    - metakey: metavalue
      empty metakey:
      another metakey: another metavalue
```

. As we can see above the value containing metadata is marked by the tag handle `!elektra/meta`. The data type contains a list with two elements. The first element of this list specifies the value of the key, while the second element contains a map saving the metadata for the key. The data above represents the following key set in Elektra if we mount the file directly to the namespace `user`:

|            Name           |  Value |     Metaname    |     Metavalue     |
|:-------------------------:|:------:|:---------------:|:-----------------:|
| user/key without metadata | value1 |        —        |         —         |
| user/key with metadata    | value2 |     metakey     |     metavalue     |
|                           |        |  empty metakey  |         —         |
|                           |        | another metakey | another metavalue |

. The example below shows how we can read and write metadata using the `yamlcpp` plugin via `kdb`.

```sh
# Mount yamlcpp plugin to cascading namespace `/examples/yamlcpp`
sudo kdb mount config.yaml user/examples/yamlcpp yamlcpp

# Manually add a key including metadata to the database
echo "🔑: !elektra/meta [🦄, {comment: Unicorn}]" >  `kdb file user/examples/yamlcpp`
kdb lsmeta user/examples/yamlcpp/🔑
#> comment
kdb getmeta user/examples/yamlcpp/🔑 comment
#> Unicorn

# Add a new key and add some metadata to the new key
kdb set user/examples/yamlcpp/brand new
kdb setmeta user/examples/yamlcpp/brand comment "The Devil And God Are Raging Inside Me"
kdb setmeta user/examples/yamlcpp/brand rationale "Because I Love It"

# Retrieve metadata
kdb lsmeta user/examples/yamlcpp/brand
#> comment
#> rationale
kdb getmeta user/examples/yamlcpp/brand rationale
#> Because I Love It

# Undo modifications to the key database
kdb rm -r user/examples/yamlcpp
sudo kdb umount user/examples/yamlcpp
```

We can also invoke additional plugins that use metadata like `type`.

```sh
sudo kdb mount config.yaml /examples/yamlcpp yamlcpp type
kdb set /examples/yamlcpp/typetest/number 21
kdb setmeta /examples/yamlcpp/typetest/number check/type short

kdb set /examples/yamlcpp/typetest/number "One"
# RET: 5
# STDERR: .*The type short failed to match for .*/number with string: One.*
# ERROR: 52

kdb get /examples/yamlcpp/typetest/number
#> 21

# Undo modifications to the key database
kdb rm -r /examples/yamlcpp
sudo kdb umount /examples/yamlcpp
```

## Binary Data

YAML CPP also supports [base64](https://tools.ietf.org/html/rfc4648) encoded data via the [Base64](../base64) plugin.

```sh
# Mount YAML CPP plugin at cascading namespace `/examples/binary`
sudo kdb mount test.yaml /examples/binary yamlcpp
# Manually add binary data
echo 'bin: !!binary aGk=' > `kdb file /examples/binary`

# Base 64 decodes the data `aGk=` to `hi` and stores the value in binary form.
# The command `kdb get` prints the data as hexadecimal byte values.
kdb get /examples/binary/bin
#> \x68\x69

# We can use `ruby` to convert the hexadecimal value returned by `kdb get`
# to its ASCII representation. If you use `bash` or `fish` as shell then
#     printf `kdb get /examples/binary/bin` # Bash
# or
#     printf (kdb get /examples/binary/bin) # fish
# should work too.
ruby -e "print ARGV[0].split('\x')[1..-1].map { |byte| byte.to_i(16).chr }.join" `kdb get /examples/binary/bin`
#> hi

# Add a string value to the database
kdb set /examples/binary/text mate
# Base 64 does not modify textual values
kdb get /examples/binary/text
#> mate

# The Base 64 plugin re-encodes binary data before YAML CPP stores the key set. Hence the
# configuration file contains the value `aGk=` even after YAML CPP wrote a new configuration.
grep -q 'bin: !.* aGk=' `kdb file user/examples/binary`
# RET: 0

# Undo modifications to the database
kdb rm -r /examples/binary
sudo kdb umount /examples/binary
```

## Dependencies

This plugin requires [yaml-cpp][]. On a Debian based OS the package for the library is called `libyaml-cpp-dev` . On macOS you can install the package `yaml-cpp` via [HomeBrew](https://brew.sh).

## Limitations

### Leaf Values

One of the limitations of this plugin is, that it only supports values inside [leaf nodes](https://github.com/ElektraInitiative/libelektra/issues/106). Let us look at an example to show what that means. The YAML file below:

```yaml
root:
  subtree:    🍂
  below root: leaf
level 1:
  level 2:
    level 3:  🍁
```

stores all of the values (`🍂`, `leaf` and `🍁`) in the leaves of the mapping. The drawing below makes this situation a little bit clearer.

![Tree](./yamlcpp/Tree.pdf)

The key set that this plugin creates using the data above looks like this (assuming we mount the plugin to `user/examples/yamlcpp`):

|     Name                                      | Value |
|-----------------------------------------------|-------|
| user/examples/yamlcpp/level                   |       |
| user/examples/yamlcpp/level 1/level 2         |       |
| user/examples/yamlcpp/level 1/level 2/level 3 | 🍁    |
| user/examples/yamlcpp/root                    |       |
| user/examples/yamlcpp/root/below root         | leaf  |
| user/examples/yamlcpp/root/subtree            | 🍂    |

. Now why is this plugin unable to store values outside leaf nodes? For example, why can we not store a value inside `user/examples/yamlcpp/level 1/level 2`? To answer this question we need to look at the YAML representation:

```yaml
level 1:
  level 2:
    level 3:  🍁
```

. In a naive approach we might just try to add a value e.g.  `🙈` right next to level 2:

```yaml
level 1:
  level 2: 🙈
    level 3:  🍁
```

. This however would be not correct, since then the YAML node `level 2` would contain both a scalar value (`🙈`) and a mapping (`{ level 3:  🍁 }`). We could solve this dilemma using a list:

```yaml
level 1:
  level 2:
    - 🙈
    - level 3:  🍁
```

. However, if we use this approach we are not able to support Elektra’s array type properly.

#### Directory Values

To overcome the limitation described above, the YAML CPP plugin requires the [Directory Value](../directoryvalue/) plugin. This plugin converts the value of a non-leaf node to a leaf node with the name `___dirdata`. For example, let us assume we have the following key set:

```
directory      = Directory Data
directory/file = Leaf Data
```

. The Directory Value plugin will convert the key set in the set (write) direction to

```
directory            =
directory/___dirdata = Directory Data
directory/file       = Leaf Data
```

. Consequently the YAML plugin will store the key set as

```yaml
directory:
  ___dirdata = Directory Data
  file       = Leaf Data
```

. A user of the YAML plugin will not notice this feature unless he edits the configuration file by hand, as the following example shows:

```sh
# Mount YAML CPP plugin at `user/examples/yamlcpp`
sudo kdb mount test.yaml user/examples/yamlcpp yamlcpp

kdb set user/examples/yamlcpp/directory 'Directory Data'
kdb setmeta user/examples/yamlcpp/directory comment 'Directory Metadata'
kdb set user/examples/yamlcpp/directory/file 'Leaf Data'

kdb ls user/examples/yamlcpp/directory
#> user/examples/yamlcpp/directory
#> user/examples/yamlcpp/directory/file

kdb get user/examples/yamlcpp/directory
#> Directory Data
kdb getmeta user/examples/yamlcpp/directory comment
#> Directory Metadata
kdb get user/examples/yamlcpp/directory/file
#> Leaf Data

# Undo modifications to the database
kdb rm -r user/examples/yamlcpp
sudo kdb umount user/examples/yamlcpp
```

.

### Other Limitations

- Saving a **single scalar value** directly below the mountpoint does not work
- Adding and removing keys does remove **comments** inside the configuration file
- The plugin currently lacks proper **type support** for scalars.

[yaml-cpp]: https://github.com/jbeder/yaml-cpp
