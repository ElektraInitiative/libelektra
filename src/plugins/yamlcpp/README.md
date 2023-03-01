- infos = Information about the yamlcpp plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = base64 directoryvalue
- infos/provides = storage/yaml
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = unittest preview unfinished concept discouraged
- infos/metadata =
- infos/description = This storage plugin reads and writes data in the YAML format

# YAML CPP

## Introduction

The YAML CPP plugin reads and writes configuration data via the [yaml-cpp][] library.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-yamlcpp`.

## Usage

You can mount this plugin via `kdb mount`:

```sh
sudo kdb mount config.yaml /tests/yamlcpp yamlcpp
```

. To unmount the plugin use `kdb umount`:

```sh
sudo kdb umount /tests/yamlcpp
```

. The following examples show how you can store and retrieve data via `yamlcpp`.

```sh
# Mount yamlcpp plugin to cascading namespace `/tests/yamlcpp`
sudo kdb mount config.yaml /tests/yamlcpp yamlcpp

# Manually add a mapping to the database
echo "🔑 : 🐳"               > `kdb file user:/tests/yamlcpp`
# Retrieve the value of the manually added key
kdb get user:/tests/yamlcpp/🔑
#> 🐳

# Manually add syntactically incorrect data
echo "some key: @some  value" >> `kdb file user:/tests/yamlcpp`
kdb get "user:/tests/yamlcpp/some key"
# STDERR: .*yaml-cpp: error at line 2, column 11: unknown token.*
# ERROR: C03100
# RET: 5

# Overwrite incorrect data
echo "🔑: value" >  `kdb file user:/tests/yamlcpp`

# Add some values via `kdb set`
kdb set user:/tests/yamlcpp 🎵
kdb set user:/tests/yamlcpp/fleetwood mac
kdb set user:/tests/yamlcpp/the chain

# Retrieve the new values
kdb get user:/tests/yamlcpp
#> 🎵
kdb get user:/tests/yamlcpp/the
#> chain
kdb get user:/tests/yamlcpp/fleetwood
#> mac

# Undo modifications
kdb rm -r /tests/yamlcpp
sudo kdb umount /tests/yamlcpp
```

## Arrays

YAML CPP provides support for Elektra’s array data type.

```sh
# Mount yamlcpp plugin to `user:/tests/yamlcpp`
sudo kdb mount config.yaml user:/tests/yamlcpp yamlcpp

# Manually add an array to the database
echo 'sunny:'       >  `kdb file user:/tests/yamlcpp`
echo '  - Charlie'  >> `kdb file user:/tests/yamlcpp`
echo '  - Dee'      >> `kdb file user:/tests/yamlcpp`

# List the array entries
kdb ls user:/tests/yamlcpp
#> user:/tests/yamlcpp/sunny
#> user:/tests/yamlcpp/sunny/#0
#> user:/tests/yamlcpp/sunny/#1

# Read an array entry
kdb get user:/tests/yamlcpp/sunny/#1
#> Dee

# You can retrieve the last index of an array by reading the metakey `array`
kdb meta get user:/tests/yamlcpp/sunny array
# 1

# Extend the array
kdb set user:/tests/yamlcpp/sunny/#2 Dennis
kdb set user:/tests/yamlcpp/sunny/#3 Frank
kdb set user:/tests/yamlcpp/sunny/#4 Mac

# The plugin supports empty array fields
kdb set user:/tests/yamlcpp/sunny/#_10 'The Waitress'
kdb meta get user:/tests/yamlcpp/sunny array
#> #_10
kdb get user:/tests/yamlcpp/sunny/#_9
# RET: 11

# Retrieve the last array entry
kdb get user:/tests/yamlcpp/sunny/$(kdb meta get user:/tests/yamlcpp/sunny array)
#> The Waitress

# The plugin also supports empty arrays (arrays without any elements)
kdb meta-set user:/tests/yamlcpp/empty array ''
kdb export user:/tests/yamlcpp/empty yamlcpp
#> []

# Arrays in Elektra always require the `array` metakey.
# Otherwise the keys will be interpreted as normal key-value mappings.
kdb set user:/tests/yamlcpp/movies ""
kdb set user:/tests/yamlcpp/movies/#0 'A Silent Voice'
kdb export user:/tests/yamlcpp/movies yamlcpp
#> "#0": A Silent Voice
kdb meta-set user:/tests/yamlcpp/movies array ''
kdb export user:/tests/yamlcpp/movies yamlcpp
#> - A Silent Voice

# Undo modifications to the key database
kdb rm -r user:/tests/yamlcpp
sudo kdb umount user:/tests/yamlcpp
```

### Nested Arrays

The plugin also supports nested arrays.

```sh
# Mount yamlcpp plugin to `user:/tests/yamlcpp`
sudo kdb mount config.yaml user:/tests/yamlcpp yamlcpp

# Add some key value pairs
kdb set user:/tests/yamlcpp/key value
kdb set user:/tests/yamlcpp/array ""
kdb set user:/tests/yamlcpp/array/#0 scalar
kdb set user:/tests/yamlcpp/array/#1/key value
kdb set user:/tests/yamlcpp/array/#1/🔑 🙈
kdb meta-set user:/tests/yamlcpp/array array '#1'

kdb ls user:/tests/yamlcpp
#> user:/tests/yamlcpp/array
#> user:/tests/yamlcpp/array/#0
#> user:/tests/yamlcpp/array/#1/key
#> user:/tests/yamlcpp/array/#1/🔑
#> user:/tests/yamlcpp/key

# Retrieve part of an array value
kdb get user:/tests/yamlcpp/array/#1/key
#> value

# Since an array saves a list of values, an array parent
# - which represent the array - does not store a value!
echo "user:/tests/yamlcpp/array: “`kdb get user:/tests/yamlcpp/array`”"
#> user:/tests/yamlcpp/array: “”

# Remove part of an array value
kdb rm user:/tests/yamlcpp/array/#1/key

kdb ls user:/tests/yamlcpp
#> user:/tests/yamlcpp/array
#> user:/tests/yamlcpp/array/#0
#> user:/tests/yamlcpp/array/#1/🔑
#> user:/tests/yamlcpp/key

# The plugin stores array keys using YAML sequences.
# Since yaml-cpp stores keys in arbitrary order -
# either `key` or `array` could be the “first” key -
# we remove `key` before we retrieve the data. This way
# we make sure that the output below will always look
# the same.
kdb rm user:/tests/yamlcpp/key
kdb file user:/tests/yamlcpp | xargs cat
#> array:
#>   - "___dirdata: "
#>   - scalar
#>   - 🔑: 🙈

# Undo modifications to the key database
kdb rm -r user:/tests/yamlcpp
sudo kdb umount user:/tests/yamlcpp
```

### Sparse Arrays

Since Elektra allows [“holes”](/doc/decisions/6_implemented/holes.md) in a key set, YAML CPP has to support small key sets that describe relatively complex data.

```sh
# Mount yamlcpp plugin
sudo kdb mount config.yaml user:/tests/yamlcpp yamlcpp

kdb set      user:/tests/yamlcpp/#0/map/#1/#0 value
kdb set      user:/tests/yamlcpp ""
kdb meta-set user:/tests/yamlcpp           array '#0'
kdb set      user:/tests/yamlcpp/#0/map ""
kdb meta-set user:/tests/yamlcpp/#0/map    array '#1'
kdb set      user:/tests/yamlcpp/#0/map/#1 ""
kdb meta-set user:/tests/yamlcpp/#0/map/#1 array '#0'
kdb file user:/tests/yamlcpp | xargs cat
#> - "___dirdata: "
#> - map:
#>     - "___dirdata: "
#>     - ~
#>     -
#>       - "___dirdata: "
#>       - value

# The plugin adds the missing array parents to the key set
kdb ls user:/tests/yamlcpp
#> user:/tests/yamlcpp
#> user:/tests/yamlcpp/#0/map
#> user:/tests/yamlcpp/#0/map/#0
#> user:/tests/yamlcpp/#0/map/#1
#> user:/tests/yamlcpp/#0/map/#1/#0

# Undo modifications to the key database
kdb rm -r user:/tests/yamlcpp
sudo kdb umount user:/tests/yamlcpp
```

## Metadata

The plugin supports metadata. The example below shows how a basic `Key` including some metadata, looks inside the YAML configuration file:

```yaml
key without metadata: value
key with metadata: !elektra/meta
  - value2
  - metakey: metavalue
    empty metakey:
    another metakey: another metavalue
```

. As we can see above the value containing metadata is marked by the tag handle `!elektra/meta`. The data type contains a list with two elements. The first element of this list specifies the value of the key, while the second element contains a map saving the metadata for the key. The data above represents the following key set in Elektra if we mount the file directly to the namespace `user`:

|            Name            | Value  |    Metaname     |     Metavalue     |
| :------------------------: | :----: | :-------------: | :---------------: |
| user:/key without metadata | value1 |        —        |         —         |
|  user:/key with metadata   | value2 |     metakey     |     metavalue     |
|                            |        |  empty metakey  |         —         |
|                            |        | another metakey | another metavalue |

. The example below shows how we can read and write metadata using the `yamlcpp` plugin via `kdb`.

```sh
# Mount yamlcpp plugin to `user:/tests/yamlcpp`
sudo kdb mount config.yaml user:/tests/yamlcpp yamlcpp

# Manually add a key including metadata to the database
echo "🔑: !elektra/meta [🦄, {comment: Unicorn}]" >  `kdb file user:/tests/yamlcpp`
kdb meta-ls user:/tests/yamlcpp/🔑
#> comment
kdb meta get user:/tests/yamlcpp/🔑 comment
#> Unicorn

# Add a new key and add some metadata to the new key
kdb set user:/tests/yamlcpp/brand new
kdb meta-set user:/tests/yamlcpp/brand comment "The Devil And God Are Raging Inside Me"
kdb meta-set user:/tests/yamlcpp/brand rationale "Because I Love It"

# Retrieve metadata
kdb meta-ls user:/tests/yamlcpp/brand
#> comment
#> rationale
kdb meta get user:/tests/yamlcpp/brand rationale
#> Because I Love It

# Undo modifications to the key database
kdb rm -r user:/tests/yamlcpp
sudo kdb umount user:/tests/yamlcpp
```

We can also invoke additional plugins that use metadata like `type`.

```sh
sudo kdb mount config.yaml user:/tests/yamlcpp yamlcpp type
kdb set user:/tests/yamlcpp/typetest/number 21
kdb meta-set user:/tests/yamlcpp/typetest/number check/type short

kdb set user:/tests/yamlcpp/typetest/number "One"
# RET: 5
# STDERR: .*Validation Semantic.*
# ERROR: C03200

kdb get user:/tests/yamlcpp/typetest/number
#> 21

# Undo modifications to the key database
kdb rm -r user:/tests/yamlcpp
sudo kdb umount user:/tests/yamlcpp
```

## Binary Data

YAML CPP also supports [base64](https://tools.ietf.org/html/rfc4648) encoded data via the [Base64](../base64) plugin.

```sh
# Mount YAML CPP plugin at `user:/tests/binary`
sudo kdb mount test.yaml user:/tests/binary yamlcpp
# Manually add binary data
echo 'bin: !!binary aGk=' > `kdb file user:/tests/binary`

# Base 64 decodes the data `aGk=` to `hi` and stores the value in binary form.
# The command `kdb get` prints the data as hexadecimal byte values.
kdb get user:/tests/binary/bin
#> \x68\x69

# Add a string value to the database
kdb set user:/tests/binary/text mate
# Base 64 does not modify textual values
kdb get user:/tests/binary/text
#> mate

# The Base 64 plugin re-encodes binary data before YAML CPP stores the key set. Hence the
# configuration file contains the value `aGk=` even after YAML CPP wrote a new configuration.
grep -q 'bin: !.* aGk=' `kdb file user:/tests/binary`
# RET: 0

# Undo modifications to the database
kdb rm -r user:/tests/binary
sudo kdb umount user:/tests/binary
```

## Empty

Sometimes you only want to save a key with an empty value. The commands below show that YAML CPP supports this scenario properly.

```sh
# Mount YAML CPP plugin at `user:/tests/yamlcpp`
sudo kdb mount test.yaml user:/tests/yamlcpp yamlcpp

# Check if the plugin saves empty keys correctly
kdb set user:/tests/yamlcpp/empty ""
kdb set user:/tests/yamlcpp/empty/level1/level2 ""

kdb ls user:/tests/yamlcpp/empty
#> user:/tests/yamlcpp/empty
#> user:/tests/yamlcpp/empty/level1/level2
kdb get -v user:/tests/yamlcpp/empty | grep -vq 'The key is null.'

# Undo modifications to the database
kdb rm -r user:/tests/yamlcpp
sudo kdb umount user:/tests/yamlcpp
```

## Binary Values

Elektra [saves binary data as either `0` or `1`](/doc/decisions/5_partially_implemented/boolean.md). The YAML CPP plugin supports this design decision by converting between YAML’s and Elektra’s boolean type.

```sh
# Mount YAML CPP plugin at `user:/tests/yamlcpp`
sudo kdb mount config.yaml user:/tests/yamlcpp yamlcpp
# Manually add boolean key
echo 'truth: true' > `kdb file user:/tests/yamlcpp`

kdb get user:/tests/yamlcpp/truth
#> 1

# A boolean in Elektra has the type `boolean`
kdb meta get user:/tests/yamlcpp/truth type
#> boolean

# Add another boolean value
kdb set user:/tests/yamlcpp/success 0
kdb meta-set user:/tests/yamlcpp/success type boolean
kdb get user:/tests/yamlcpp/success
#> 0
kdb export user:/tests/yamlcpp/success yamlcpp
#> false

# Undo modifications to the database
kdb rm -r user:/tests/yamlcpp
sudo kdb umount user:/tests/yamlcpp
```

## Dependencies

This plugin requires [yaml-cpp][]. On a Debian based OS the package for the library is called [`libyaml-cpp-dev`](https://packages.debian.org/libyaml-cpp-dev). On macOS you can install the package [`yaml-cpp`](https://repology.org/project/yaml-cpp) via [HomeBrew](https://brew.sh).

## Limitations

### Leaf Values

One of the limitations of this plugin is, that it only supports values inside [leaf nodes](https://github.com/ElektraInitiative/libelektra/issues/106). Let us look at an example to show what that means. The YAML file below:

```yaml
root:
  subtree: 🍂
  below root: leaf
level 1:
  level 2:
    level 3: 🍁
```

stores all of the values (`🍂`, `leaf` and `🍁`) in the leaves of the mapping. The drawing below makes this situation a little bit clearer.

![Tree](./yamlcpp/Tree.pdf)

The key set that this plugin creates using the data above looks like this (assuming we mount the plugin to `user:/tests/yamlcpp`):

| Name                                        | Value |
| ------------------------------------------- | ----- |
| user:/tests/yamlcpp/level                   |       |
| user:/tests/yamlcpp/level 1/level 2         |       |
| user:/tests/yamlcpp/level 1/level 2/level 3 | 🍁    |
| user:/tests/yamlcpp/root                    |       |
| user:/tests/yamlcpp/root/below root         | leaf  |
| user:/tests/yamlcpp/root/subtree            | 🍂    |

. Now why is this plugin unable to store values outside leaf nodes? For example, why can we not store a value inside `user:/tests/yamlcpp/level 1/level 2`? To answer this question we need to look at the YAML representation:

```yaml
level 1:
  level 2:
    level 3: 🍁
```

. In a naive approach we might just try to add a value e.g. `🙈` right next to level 2:

```yaml
level 1:
  level 2: 🙈
    level 3:  🍁
```

. This however would be not correct, since then the YAML node `level 2` would contain both a scalar value (`🙈`) and a mapping (`{ level 3: 🍁 }`). We could solve this dilemma using a list:

```yaml
level 1:
  level 2:
    - 🙈
    - level 3: 🍁
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
  ___dirdata: Directory Data
  file: Leaf Data
```

. A user of the YAML plugin will not notice this feature unless he edits the configuration file by hand, as the following example shows:

```sh
# Mount YAML CPP plugin at `user:/tests/yamlcpp`
sudo kdb mount test.yaml user:/tests/yamlcpp yamlcpp

kdb set user:/tests/yamlcpp/directory 'Directory Data'
kdb meta-set user:/tests/yamlcpp/directory comment 'Directory Metadata'
kdb set user:/tests/yamlcpp/directory/file 'Leaf Data'

kdb ls user:/tests/yamlcpp/directory
#> user:/tests/yamlcpp/directory
#> user:/tests/yamlcpp/directory/file

kdb get user:/tests/yamlcpp/directory
#> Directory Data
kdb meta get user:/tests/yamlcpp/directory comment
#> Directory Metadata
kdb get user:/tests/yamlcpp/directory/file
#> Leaf Data

# Undo modifications to the database
kdb rm -r user:/tests/yamlcpp
sudo kdb umount user:/tests/yamlcpp
```

.

### Special Values

Due to the way the plugin writes data

- first converting the key set into yaml-cpp’s `Node` data structure, and then
- writing this data structure into a file,

and the way the yaml-cpp library handles writing `Nodes`, the plugin does currently not handle data with special meaning according to the [YAML spec](https://yaml.org/spec/1.2/spec.html) correctly. For example, if you use the `kdb` tool to save the value `true` in a key, then the plugin will not quote this value and you will end up with a boolean value.

```sh
# Mount plugin
sudo kdb mount test.yaml user:/tests/yamlcpp yamlcpp

kdb set user:/tests/yamlcpp/boolean true
# The following command should print a quoted YAML scalar
# (e.g. `"true"` or `'true'`).
kdb export user:/tests/yamlcpp/boolean yamlcpp
#> true

# Since the value is not quoted the YAML CPP plugin will
# correctly convert the YAML data into one of Elektra’s
# boolean values (`0` or `1`).
kdb get user:/tests/yamlcpp/boolean
#> 1

# Undo modifications to the database
kdb rm -r user:/tests/yamlcpp
sudo kdb umount user:/tests/yamlcpp
```

### Other Limitations

- Adding and removing keys does remove **comments** inside the configuration file
- The plugin currently lacks proper **type support** for scalars.
- If Elektra uses YAML CPP as **default storage** plugin, multiple tests of the test suite fail. However, if you mount YAML CPP at `/`:

  ```
  kdb mount default.yaml / yamlcpp
  ```

  all tests should work correctly. The problem here is that Elektra does not load additional required plugins (`infos/needs`) for a
  default storage plugin.

[yaml-cpp]: https://github.com/jbeder/yaml-cpp
