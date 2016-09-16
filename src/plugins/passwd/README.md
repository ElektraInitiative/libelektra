- infos = Information about the passwd plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained reviewed conformant compatible coverage specific unittest tested nodep libc configurable experimental
- infos/metadata =
- infos/description =

## Introduction ##

This plugin parses `passwd` files, e.g. `/etc/passwd`. 

## Implementation Details ##

If present, the not-posix compliant `fgetpwent` function will be used to read the file supplied by the resolver, otherwise `getpwent` will be used. For writing, if present, `putpwent` will be used, if not a simple implementation writing straight to the config file.

## Configuration ##

If the config key `index` is set to `name` passwd entrys will be sorted by name, if not set or set to `uid` passwd entries will be sorted by uid

## Fields ##

- `gecos` contains the full name of the account
- `gid` contains the accounts primary group id
- `home` contains the path to the accounts home directoy
- `shell` contains the accounts default shell
- `uid` contains the accounts uid
- `name` contains the account name

## Usage ##

```
kdb mount /etc/passwd system/passwd passwd index=name
kdb export system/passwd/root

gecos = root
gid = 0
home = /root
passwd = x
shell = /bin/zsh
uid = 0
```
