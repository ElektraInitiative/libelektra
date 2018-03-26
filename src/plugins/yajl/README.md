- infos = Information about YAJL plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/json
- infos/needs = directoryvalue
- infos/recommends = rebase comment type
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

The JSON grammar can be found [here](http://www.ietf.org/rfc/rfc4627.txt).

A validator can be found [here](http://jsonlint.com/).

Supports every KeySet except when arrays are intermixed with other keys.
Has only limited support for metadata.

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
the warning `#78` will be added because of the potential
data loss.

## Special values

In JSON it is possible to have empty arrays and objects.
In Elektra this is mapped using the special names

    ###empty_array

and

    ___empty_map

Arrays are mapped to Elektra’s array convention #0, #1,..

## Restrictions

- Only UTF-8 is supported. Use the `iconv` plugin if your locale are
  not UTF-8. When using non-UTF-8 the plugin will be able to write
  the file, but cannot parse it back again. You will error #77,
  invalid bytes in UTF8 string.
- Everything is string if not tagged by metakey "type"
  Only valid JSON types can be used in type, otherwise there are some
  fall backs to string but warnings are produced.
- Arrays will be normalized (to #0, #1, ..)
- Comments of various JSON-dialects are discarded.
- Mixing of arrays and maps is not detected and leads to corrupted
  JSON files. Please specify arrays to avoid such situations.

Because of these potential problems a type checker
and comments filter are highly recommended.

## Usage

The following example shows you how you can read and write data using this plugin.

```sh
# Mount the plugin to the cascading namespace `/examples/yajl`
sudo kdb mount config.json /examples/yajl yajl

# Manually add a key-value pair to the database
printf '{ "number": 1337 }' > `kdb file /examples/yajl`

# Retrieve the new value
kdb get /examples/yajl/number
#> 1337

# Determine the data type of the value
kdb getmeta /examples/yajl/number type
#> double

# Add another key-value pair
kdb set /examples/yajl/key value
#> Using name user/examples/yajl/key
#> Create a new key user/examples/yajl/key with string "value"

# Retrieve the new value
kdb get /examples/yajl/key
#> value

# Check the format of the configuration file
kdb file user/examples/yajl/ | xargs cat
#> {
#>     "key": "value",
#>     "number": 1337
#> }

# Add an array
kdb set user/examples/yajl/piggy/#0 straw
kdb set user/examples/yajl/piggy/#1 sticks
kdb set user/examples/yajl/piggy/#2 bricks

# Retrieve an array key
kdb get user/examples/yajl/piggy/#2
#> bricks

# Check the format of the configuration file
kdb file user/examples/yajl | xargs cat
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
kdb rm -r /examples/yajl
sudo kdb umount /examples/yajl
```

### Directory Values

The YAJL plugin support values in directory keys via the [Directory Value](../directoryvalue/) plugin.

```sh
# Mount the plugin to the cascading namespace `/examples/yajl`
sudo kdb mount config.json /examples/yajl yajl

# Add two directory keys and one leaf key
kdb set /examples/yajl/roots 'Things Fall Apart'
kdb set /examples/yajl/roots/bloody 'Radical Face'
kdb set /examples/yajl/roots/bloody/roots 'No Roots'

# Add an array containing two elements
kdb set /examples/yajl/now ', Now'
kdb set /examples/yajl/now/#0 'Neighbors'
kdb set /examples/yajl/now/#1 'Threads'

kdb ls /examples/yajl
#> user/examples/yajl
#> user/examples/yajl/now
#> user/examples/yajl/now/#0
#> user/examples/yajl/now/#1
#> user/examples/yajl/roots
#> user/examples/yajl/roots/bloody
#> user/examples/yajl/roots/bloody/roots

# Retrieve directory values
kdb get /examples/yajl/roots
#> Things Fall Apart
kdb get /examples/yajl/roots/bloody
#> Radical Face

# Retrieve leaf value
kdb get /examples/yajl/roots/bloody/roots
#> No Roots

# Check array
kdb get /examples/yajl/now
#> , Now
kdb getmeta /examples/yajl/now array
#> #1
kdb get /examples/yajl/now/#0
#> Neighbors
kdb get /examples/yajl/now/#1
#> Threads

# Undo modifications to the database
kdb rm -r /examples/yajl
sudo kdb umount /examples/yajl
```

## OpenICC Device Config

This plugin was specifically designed and tested for the
`OpenICC_device_config_DB` although it is of course not limited
to it.

Mount the plugin:

    kdb mount --resolver=resolver_fm_xhp_x color/settings/openicc-devices.json /org/freedesktop/openicc yajl rename cut=org/freedesktop/openicc

or:

    kdb mount-openicc

Then you can copy the OpenICC_device_config_DB.json
to systemwide or user config, e.g.

    cp src/plugins/yajl/examples/OpenICC_device_config_DB.json /etc/xdg
    cp src/plugins/yajl/examples/OpenICC_device_config_DB.json ~/.config

    kdb ls system/org/freedesktop/openicc

prints out then all device entries available in the config

    kdb get system/org/freedesktop/openicc/device/camera/0/EXIF_manufacturer

prints out "Glasshuette" with the example config in source

You can export the whole system openicc config to ini with:

    kdb export system/org/freedesktop/openicc simpleini > dump.ini

or import it:

    kdb import system/org/freedesktop/openicc ini < dump.ini

