- infos = Information about the gitresolver plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = resolver
- infos/recommends =
- infos/placements = rollback getresolver setresolver commit
- infos/status = recommended productive reviewed conformant compatible coverage shelltest tested libc configurable final preview nodoc
- infos/features/storage = limited
- infos/metadata =
- infos/description = resolver for Git repositories

## Description

gitresolver is a resolver that fetches from a local Git repository during the get-phase and commits them back at the end of the set-phase.
It operates on a temporary copy of the latest version of your file fetched from the repository. If the temporary copy modified, a new commit with the modified version will be created. Local files won't be touched.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-gitresolver`.

## Options

`branch` defines the branch to work on. Default: master
`tracking` can be either `object` or `head` (default). if set to `object` a conflict will only occur if the file in the Git repository has been updated while you were working on it. `head` will cause a conflict if the `HEAD` commit has been updated.
`pull` when present: tells the plugin to fast-forward pull the repository, fails if FF isn't possible.
`checkout` when present: tells the plugin to checkout the file.

## Limitations

Currently it only works inside existing Git repositories.

## Examples

```
sudo kdb mount -R gitresolver /path/to/my/gitrepo/file.ini system:/gittest ini shell \
    execute/set='cd /path/to/my/gitrepo/ && git commit --amend'
```
