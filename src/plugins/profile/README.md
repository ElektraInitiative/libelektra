- infos = Information about the profile plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained libc preview experimental unfinished nodoc global 
- infos/metadata =
- infos/description =

## Usage ##

Following the elektra keyname convention application configurations are stored under `/sw/org/myapp/#` and `/sw/org/myapp/#0/current`is the profile to be used.
The `profile` plugin provides an easy way to switch configuration profiles. 

The key `/sw/org/myapp/#0/profile` defines what profile should be used as `current`, e.g. `/sw/org/myapp/#0/profile = myprofile`.
If a key `/sw/org/myapp/#0/myprofile/key` is found and no key `/sw/org/myapp/#0/current/key` exists an override key will be created linking `/sw/org/myapp/#0/currrent/key` to `/sw/org/myapp/#0/myapp/key` 
If neither `/sw/org/myapp/#0/current/key` nor `/sw/org/myapp/#0/myprofile/key` is found, but `/sw/org/myapp/#0/%/key`, `/sw/org/myapp/#0/current/key` will be linked to `/sw/org/myapp/#0/%/key`


### EXAMPLE ###

% cat profile.ini
```
[sw/org/myapp/#0]
profile = myprofile

[sw/org/myapp/#0/current]
key2 = stillHere?

[sw/org/myapp/#0/myprofile]
key1 = test1
key2 = test2

[sw/org/myapp/#0/%]
key2 = failed?
key3 = test3
```

% kdb ls /sw
```
spec/sw/org/myapp/#0/current/key1
spec/sw/org/myapp/#0/current/key3
user/sw/org/myapp/#0
user/sw/org/myapp/#0/%
user/sw/org/myapp/#0/%/key2
user/sw/org/myapp/#0/%/key3
user/sw/org/myapp/#0/current
user/sw/org/myapp/#0/current/key2
user/sw/org/myapp/#0/profile
user/sw/org/myapp/#0/myprofile
user/sw/org/myapp/#0/myprofile/key1
user/sw/org/myapp/#0/myprofile/key2
```

% kdb get -v /sw/org/myapp/#0/current/key1
```
got 25 keys
searching spec/sw/org/myapp/#0/current/key1, found: spec/sw/org/myapp/#0/current/key1, options: KDB_O_CALLBACK
The resulting keyname is user/sw/org/myapp/#0/myprofile/key1
test1
```
