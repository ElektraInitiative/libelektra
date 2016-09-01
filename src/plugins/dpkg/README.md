- infos = Information about the dpkg plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/status = nodoc unfinished nodep
- infos/description =

## Example ##

```
kdb mount /var/lib/dpkg/available system/dpkg/available dpkg
kdb mount /var/lib/dpkg/status system/dpkg/available dpkg
```
