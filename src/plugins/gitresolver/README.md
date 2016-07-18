- infos = Information about the gitresolver plugin is in keys below
- infos/author = Name <name@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = resolver
- infos/recommends =
- infos/placements = rollback getresolver setresolver commit
- infos/status = recommended productive maintained reviewed conformant compatible coverage specific unittest shelltest tested nodep libc configurable final preview 
- infos/metadata =
- infos/description =

## Usage ##

```
kdb mount -R gitresolver /path/to/my/gitrepo/file.ini system/gittest ini shell execute/set='cd /path/to/my/gitrepo/ && git commit --amend'
```
