- infos = Information about SIMPLEINI plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/ini
- infos/needs = code null
- infos/placements = getstorage setstorage
- infos/status = maintained configurable concept obsolete 3000
- infos/description = Very simple storage which writes out in a basic ini format.

## Introduction ##

This plugin reads and writes files written in a basic line-oriented ini-like format.
It is very simplistic without sections, the [ini](../ini/) plugin and for specifications
the [ni](../ni/) plugin should be preferred in most cases.

## Usage ##

It is quite suitable to export configuration if you want line-by-line key, value pairs
without sections or metadata.
(Thus +3000 in status)

    $ kdb export system/samba simpleini

## Configuration ##

The only parameter simpleini supports is `format` which allows you to change the syntax
of individual lines.
The `format` is a string with any characters where only `%` has special meaning:

- `%` in an even number N are escaped and represent N/2 `%`, e.g. `%%%%` are actually `%%` in the resulting format.
- The first unescaped `%` represents the key.
- The second unescaped `%` represents the value.

The default is `% = %`.

For example, if you want every key to be marked `%:key value` you would use:

    $ kdb export -c "format=%%:% %" system/samba simpleini
    %:key value
    %:key2 value2

## Restrictions ##

- Lines in a different format (e.g. comments) are discarded.
- The order per line must be key and then value: the plugin cannot be used if the value is first
- Spaces are trimmed
- Delimiting symbols cannot be part of the key.
- The last occurrence of the same key wins (others are discarded).
- The parent Key cannot be used.
- This plugin needs the code and null plugins.
  A code plugin is used for the escape character for some symbols (but does not respect user-defined `format`)
  and the null plugin is used to handle null values.

## Examples ##

Mount the plugin:

    $ kdb mount -d /etc/samba/smb.conf system/samba ccode null simpleini

