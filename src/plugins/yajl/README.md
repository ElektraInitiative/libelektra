- infos = Information about YAIL plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/json
- infos/needs =
- infos/recommends = rebase directoryvalue comment type
- infos/placements = getstorage setstorage
- infos/status = maintained coverage unittest
- infos/description = JSON using YAIL

## Introduction ##

This is a plugin reading and writing json files
using the library [yail](http://lloyd.github.com/yajl/)

The plugin was tested with yajl version 1.0.8-1 from Debian 6
and yajl version 2.0.4-2 from Debian 7.

Examples of files which are used for testing can be found
below the folder in "src/plugins/yajl/examples".

The json grammar can be found [here](http://www.ietf.org/rfc/rfc4627.txt).

A validator can be found [here](http://jsonlint.com/).

Supports every KeySet except when arrays are intermixed with other keys.
Has only limited support for metadata.

## Dependencies ##

- `libyajl-dev` (version 1 and 2 should work)

## Types ##

My metadata `type` the used types can be chosen:

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

## Special values ##

In json it is possible to have empty arrays and objects.
In Elektra this is mapped using the special names

        ###empty_array

and

        ___empty_map

Arrays are mapped to Elektra's array convention #0, #1,..


## Restrictions ##

- Everything is string if not tagged by meta key "type"
  Only valid json types can be used in type, otherwise there are some
  fall backs to string but warnings are produced.
- Values in non-leaves are discarded.
- Arrays will be normalized (to #0, #1, ..)
- Comments of various json-dialects are discarded.

Because of these potential problems a type checker,
comments filter and directory value filter are highly recommended.



## OpenICC Device Config ##


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

prints out "Glasshuette" with the example config in souce

You can export the whole system openicc config to ini with:

        kdb export system/org/freedesktop/openicc simpleini > dump.ini

or import it:

        kdb import system/org/freedesktop/openicc ini < dump.ini
