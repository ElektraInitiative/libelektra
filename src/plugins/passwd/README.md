- infos = Information about the passwd plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/passwd
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained reviewed conformant compatible coverage specific unittest tested nodep libc configurable experimental limited
- infos/metadata =
- infos/description = storage plugin for passwd files

## Introduction

This plugin parses `passwd` files, e.g. `/etc/passwd`.

## Implementation Details

The non-POSIX function `fgetpwent` (GNU_SOURCE) will be used to
read the file supplied by the resolver.
As a fallback we implemented our own version based on musls `fgetpwent`.

For writing `putpwent` (GNU_SOURCE) will be used.
If it is not available the plugin will write straight to the config file.

## Requirements

For the plugin to be build at least `POSIX_C_SOURCE >= 200809L` compatibility
is required.

## Configuration

If the config key `index` is set to `name` passwd entries will be sorted by name, if not set or set to `uid` passwd entries will be sorted by uid

## Fields

- `gecos` contains the full name of the account
- `gid` contains the accounts primary group id
- `home` contains the path to the accounts home directory
- `shell` contains the accounts default shell
- `uid` contains the accounts uid
- `name` contains the account name

## Usage

```sh
kdb mount /etc/passwd system/passwd passwd index=name
kdb export system/passwd/root
#> gecos = root
#> gid = 0
#> home = /root
#> passwd = x
#> shell = /bin/zsh
#> uid = 0
```
