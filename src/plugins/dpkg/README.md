- infos = Information about the dpkg plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/dpkg
- infos/placements = getstorage setstorage
- infos/status = nodep limited nodoc unfinished
- infos/description = can be used to mount dpkg files

## Example

```sh
kdb mount /var/lib/dpkg/available system/dpkg/available dpkg
kdb mount /var/lib/dpkg/status system/dpkg/available dpkg
```
