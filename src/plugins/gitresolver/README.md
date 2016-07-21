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

## Description ##

gitresolver is a resolver that fetches from a local git repository during the get-phase and commits them back at the end of the set-phase.
It operates on a temporary copy of the latest version of your file fetched from the repository. If the temporary copy modified, a new commit with the modified version will be created. Local files won't be touched.

## Options ##

`branch` defines the branch to work on. Default: master
`tracking` can be either `object` or `head` (default). if set to `object` a conflict will only occur if the file in the git repository has been updated while you were working on it. `head` will cause a conflict if the `HEAD` commit has been updated. 

## Limitations ##

Currently it only works on already existing files inside existing git repositories. 


## Examples ##

```
kdb mount -R gitresolver /path/to/my/gitrepo/file.ini system/gittest ini shell execute/set='cd /path/to/my/gitrepo/ && git commit --amend'
```
