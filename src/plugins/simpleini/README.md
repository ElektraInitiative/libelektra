- infos = Information about simpleini plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/properties
- infos/needs = code binary
- infos/placements = getstorage setstorage
- infos/status = maintained tested/unit nodep concept obsolete 3000
- infos/description = Very simple storage plugin which stores data in a basic properties file format

## Introduction

This plugin reads and writes files written in a basic line-oriented ini-like format.
It is very simplistic without sections, the [toml](../toml/) plugin and for specifications
the [ni](../ni/) plugin should be preferred. Since the `simpleini` plugin requires
the GNU C library it **will not work** on operating systems that use another C library
such as macOS.

## Usage

It is quite suitable to export configuration if you want line-by-line key, value pairs
without sections or metadata.
(Thus +3000 in status)

```sh
kdb export system:/samba simpleini
```

## Configuration

The only parameter simpleini supports is `format` which allows you to change the syntax
of individual lines.
The `format` is a string with any characters where only `%` has special meaning:

- `%` in an even number N are escaped and represent N/2 `%`, e.g. `%%%%` are actually `%%` in the resulting format.
- The first unescaped `%` represents the key.
- The second unescaped `%` represents the value.

The default is `% = %`.

For example, if you want every key to be marked `%:key value` you would use:

```sh
kdb export -c "format=%%:% %" system:/samba simpleini
#> %:key value
#> %:key2 value2
```

## Restrictions

- Lines in a different format (e.g. comments) are discarded.
- The order per line must be key and then value: the plugin cannot be used if the value is first
- Whitespace before and after keynames are trimmed (but not for value)
- Delimiting symbols cannot be part of the key.
- The last occurrence of the same key wins (others are discarded).
- The parent Key cannot be used.
- This plugin needs the code and binary plugins.
  A code plugin is used for the escape character for some symbols (but does not respect user-defined `format`)
  and the binary plugin is used to handle binary values.

## Examples

Mount the plugin:

```sh
kdb mount -d /etc/samba/smb.conf system:/samba ccode simpleini
```

## Limitations

- will be excluded in macOS
- cannot parse entries where key or value is missing
