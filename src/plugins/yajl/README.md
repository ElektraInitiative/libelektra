- infos = Information about YAJL plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/json
- infos/needs = directoryvalue type
- infos/recommends = rebase comment
- infos/placements = getstorage setstorage
- infos/status = maintained coverage unittest
- infos/description = JSON using YAJL

## Introduction

This is a plugin reading and writing JSON files
using the library [yail](http://lloyd.github.com/yajl/)

The plugin was tested with yajl version 1.0.8-1 from Debian 6
and yajl version 2.0.4-2 from Debian 7.

Examples of files which are used for testing can be found
below the folder in "src/plugins/yajl/yajl".

The JSON grammar can be found [here](http://www.json.org).

A validator can be found [here](http://jsonlint.com/).

Supports every KeySet except when arrays are intermixed with other keys.
Has only limited support for metadata.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-yajl`.

## Dependencies

- `libyajl-dev` (version 1 and 2 should work)

## Types

The type of the data is available via the metadata `type`:

- `string`:
  The JSON string type.
- `boolean`:
  The JSON boolean type (true or false)
- `double`:
  For JSON numbers.

If no metadata `type` is given, the type is either:

- `null` on binary null-key
- `string` otherwise

Any other type/value will still be treated as string, but
the warning `C03200` will be added because of the potential
data loss.

## Special values

In JSON it is possible to have empty arrays and objects.
In Elektra this is mapped using the special names

```
###empty_array
```

and

```
___empty_map
```

Arrays are mapped to Elektra’s array convention #0, #1,..

## Restrictions

- Only UTF-8 is supported. Use the `iconv` plugin if your locale are
  not UTF-8. When using non-UTF-8 the plugin will be able to write
  the file, but cannot parse it back again. You will error C03100,
  invalid bytes in UTF8 string.
- Everything is string if not tagged by metakey "type"
  Only valid JSON types can be used in type, otherwise there are some
  fall backs to string but warnings are produced.
- Arrays will be normalized (to #0, #1, ..)
- Comments of various JSON-dialects are discarded.
- Mixing of arrays and maps is not detected and leads to corrupted
  JSON files. Please specify arrays to avoid such situations.
- The plugin creates adds an empty root key to the database, even if you
  did not add this key (see http://issues.libelektra.org/2132).

Because of these potential problems a type checker
and comments filter are highly recommended.

## Usage

The following example shows you how you can read and write data using this plugin.

```sh
# Mount the plugin to the cascading namespace `/tests/yajl`
sudo kdb mount config.json /tests/yajl yajl

# Manually add a key-value pair to the database
printf '{ "number": 1337 }' > `kdb file user:/tests/yajl`

# Retrieve the new value
kdb get user:/tests/yajl/number
#> 1337

# Determine the data type of the value
kdb meta get user:/tests/yajl/number type
#> double

# Add another key-value pair
kdb set user:/tests/yajl/key value
# STDOUT-REGEX: .*Create a new key user:/tests/yajl/key with string "value"

# Retrieve the new value
kdb get user:/tests/yajl/key
#> value

# Check the format of the configuration file
kdb file user:/tests/yajl/ | xargs cat
#> {
#>     "key": "value",
#>     "number": 1337
#> }

# Add an array
kdb set user:/tests/yajl/piggy/#0 straw
kdb set user:/tests/yajl/piggy/#1 sticks
kdb set user:/tests/yajl/piggy/#2 bricks

# Retrieve an array key
kdb get user:/tests/yajl/piggy/#2
#> bricks

# Check the format of the configuration file
kdb file user:/tests/yajl | xargs cat
#> {
#>     "key": "value",
#>     "number": 1337,
#>     "piggy": [
#>         "straw",
#>         "sticks",
#>         "bricks"
#>     ]
#> }

# Undo modifications to the database
kdb rm -r user:/tests/yajl
sudo kdb umount /tests/yajl
```

### Directory Values

The YAJL plugin support values in directory keys via the [Directory Value](../directoryvalue/) plugin.

```sh
# Mount the plugin to `user:/tests/yajl`
sudo kdb mount config.json user:/tests/yajl yajl

# Add two directory keys and one leaf key
kdb set user:/tests/yajl/roots 'Things Fall Apart'
kdb set user:/tests/yajl/roots/bloody 'Radical Face'
kdb set user:/tests/yajl/roots/bloody/roots 'No Roots'

# Add an array containing two elements
kdb set user:/tests/yajl/now ', Now'
# Elektra arrays require the metakey `array` to the parent.
# Otherwise the keys below `user:/tests/yajl/now` would be
# interpreted as normal key-value pairs.
kdb meta-set user:/tests/yajl/now array ''
kdb set user:/tests/yajl/now/#0 'Neighbors'
kdb set user:/tests/yajl/now/#1 'Threads'

kdb ls user:/tests/yajl
#> user:/tests/yajl/now
#> user:/tests/yajl/now/#0
#> user:/tests/yajl/now/#1
#> user:/tests/yajl/roots
#> user:/tests/yajl/roots/bloody
#> user:/tests/yajl/roots/bloody/roots

# Retrieve directory values
kdb get user:/tests/yajl/roots
#> Things Fall Apart
kdb get user:/tests/yajl/roots/bloody
#> Radical Face

# Retrieve leaf value
kdb get user:/tests/yajl/roots/bloody/roots
#> No Roots

# Check array
kdb get user:/tests/yajl/now
#> , Now
kdb meta get user:/tests/yajl/now array
#> #1
kdb get user:/tests/yajl/now/#0
#> Neighbors
kdb get user:/tests/yajl/now/#1
#> Threads

# Undo modifications to the database
kdb rm -r user:/tests/yajl
sudo kdb umount user:/tests/yajl
```

### Booleans

The YAJL plugin maps "1" and "true" to its true bool type, and "0" and "false" to its false bool type.
However, it always returns 1 or 0.

You can take advantage of the [type](../type/README.md) plugin to map arbitrary values to true and false.

```sh
# Type plugin is automatically mounted since yajl depends on it
sudo kdb mount conf.json user:/tests/yajl yajl
kdb set user:/tests/yajl 1
kdb get user:/tests/yajl
#> 1
kdb meta-set user:/tests/yajl type boolean
kdb set user:/tests/yajl on
kdb get user:/tests/yajl
#> 1
kdb set user:/tests/yajl/subkey disable
kdb meta-set user:/tests/yajl/subkey type boolean
kdb get user:/tests/yajl/subkey
#> 0

# Undo modifications to the database
kdb rm -r user:/tests/yajl
sudo kdb umount user:/tests/yajl
```

## OpenICC Device Config

This plugin was specifically designed and tested for the
`OpenICC_device_config_DB` although it is of course not limited
to it.

Mount the plugin:

```bash
kdb mount --resolver=resolver_fm_xhp_x color/settings/openicc-devices.json \
  /org/freedesktop/openicc yajl rename cut=org/freedesktop/openicc
```

or:

```bash
kdb mount-openicc
```

Then you can copy the `OpenICC_device_config_DB.json`
to systemwide or user config, e.g.

```bash
cp src/plugins/yajl/examples/OpenICC_device_config_DB.json /etc/xdg
cp src/plugins/yajl/examples/OpenICC_device_config_DB.json ~/.config

kdb ls system:/org/freedesktop/openicc
```

prints out then all device entries available in the config

```bash
kdb get system:/org/freedesktop/openicc/device/camera/0/EXIF_manufacturer
```

prints out "Glasshuette" with the example config in source

You can export the whole system openicc config to ini with:

```bash
kdb export system:/org/freedesktop/openicc simpleini > dump.ini
```

or import it:

```bash
kdb import system:/org/freedesktop/openicc ini < dump.ini
```
