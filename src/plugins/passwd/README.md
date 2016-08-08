- infos = Information about the passwd plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = recommended productive maintained reviewed conformant compatible coverage specific unittest shelltest tested nodep libc configurable final preview memleak experimental difficult unfinished old nodoc concept orphan obsolete discouraged -1000000
- infos/metadata =
- infos/description =

## Introduction ##

This plugin parses `passwd` files, e.g. `/etc/passwd`. If present, the not-posix complient `fgetpwent` function will be used to read the file supplied by the resolver, if not, `getpwent` will be used. For writing, if present, `putpwent` will be used, if not a simple implementation writing straight to the config file.

## Configuration ##

If the config key `index` is set to `name` passwd entrys will be sorted by name, if not set or set to `uid` passwd entry will be sorted by uid

## Fields ##

`gecos` contains the full name of the account
`gid` contains the accounts primary group id
`home` contains the path to the accounts home directoy
`shell` contains the accounts default shell
`uid` contains the accounts uid
`name` contains the account name

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
