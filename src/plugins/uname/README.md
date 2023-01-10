- infos = Information about the uname plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/info
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = maintained unittest shelltest nodep concept
- infos/features/storage = read limited
- infos/description = Includes uname information into the key database.

## Introduction

This plugin is a storage plugin that will use the syscall `uname (2)`.
No resolver is needed for that plugin to work.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Special Values

This plugin defines following keynames below its mount point:

- sysname
- nodename
- release
- version
- machine

## Errors

The only documented error in `uname(2)` is when an invalid buffer is passed to it.
As this is an implementation error only, this plugin should not run into errors.

## Restrictions

This plugin is read-only.

## Example

```sh
# To mount uname information using this plugin:
kdb mount -R noresolver none user:/tests/uname uname

# List available data
kdb ls user:/tests/uname/
#> user:/tests/uname/machine
#> user:/tests/uname/nodename
#> user:/tests/uname/release
#> user:/tests/uname/sysname
#> user:/tests/uname/version

# Read the OS name
kdb get user:/tests/uname/sysname
# STDOUT-REGEX: CYGWIN_NT.*|Darwin|DragonFly|FreeBSD|Linux|OpenBSD

# Read the OS version number
kdb get user:/tests/uname/release
# STDOUT-REGEX: [0-9]+(\.[0-9]+)*[[:alnum:][:punct:]]*

# Unmount the plugin
kdb umount user:/tests/uname
```
