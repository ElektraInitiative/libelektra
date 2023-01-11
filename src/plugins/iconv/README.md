- infos = Information about iconv plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = conv
- infos/needs =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained tested/unit
- infos/description = Converts values of keys between charsets

## Introduction

This plugin is a filter plugin that converts between different character encodings.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Purpose

Consider a user insisting on a `latin1` character encoding because
of some old application. All other users already use, for example,
`UTF-8`. For these users, the configuration files are encoded in
`UTF-8`. So we need a solution for the user with `latin1` to access the
key database with proper encoding.

On the other hand, contemplate an XML file which requires a specific
encoding. But the other key databases work well with the users
encoding. So a quick fix for that backend is needed to feed that XML
file with a different encoding.

The iconv plugin provides a solution for both scenarios. It converts between
many available character encodings. With the plugin’s configuration
the user can change the from and to encoding. The default values of the
plugin configuration are: `from` encoding will be determined at run-time.
`to` encoding is `UTF-8`.

Parameters:

- `to` is per default UTF-8, to have unicode configuration files
- `from` is per default the users locale

Note that for writing the configuration `from` and `to` is swapped. A
key database that requires a specific encoding can make use of it. To
sum up, every user can select a different encoding, but the key databases
are still properly encoded for anyone.

## Example

For example `iconv/iconv.ini` should be `latin1`, but all users
have `UTF-8` settings:

```sh
# Mount the file `iconv/iconv.ini` using the `mini` plugin together with `iconv`
sudo kdb mount "$PWD/src/plugins/iconv/iconv/iconv.ini" system:/tests/iconv mini iconv from=UTF-8,to=ISO-8859-1

# Check the file type of the mounted file
file -b "`kdb file system:/tests/iconv`"
#> ISO-8859 text

kdb get system:/tests/iconv/a         # converts ISO-8859 to UTF-8
#> hellö

kdb set system:/tests/iconv/a öäß     # converts UTF-8 to ISO-8859
kdb get system:/tests/iconv/a
#> öäß

# Cleanup
kdb set system:/tests/iconv/a hellö
sudo kdb umount system:/tests/iconv
```
