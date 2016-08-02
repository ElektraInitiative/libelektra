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

## Usage ##

```
kdb mount /etc/passwd system/passwd passwd index=name
```
if `index` is set to `name` passwd entrys will be sorted by name, if not set or set to `uid` passwd entry will be sorted by uid
