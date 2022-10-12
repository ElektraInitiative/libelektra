- infos = Information about the backend plugin is in the keys below
- infos/author = Vid Leskovar <vid.leskovar5@gmail.com>, Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/provides = backend
- infos/needs =
- infos/recommends =
- infos/placements = backend
- infos/status = nodep
- infos/description = Plugin implementing full backend functionality

## Introduction

This plugin implements the full functionality of a backend.

## Example

The example below is written with cascading keys, but the keys should be interpreted as relative to a mountpoint configuration root (e.g. `system:/elektra/mountpoints/\/hosts`).

> **Note**: The `pre*` and `post*` positions are not proper arrays.
> The plugin will use all keys that are _directly below_ e.g. `/positions/get/poststorage` no matter their names.
> Using array-like names is just a convenient way to get a predictable ordering.

```
# Part 1: Generic part defined by libelektra-kdb
/plugins/#0/name (="resolver_fm_hpu_b")
/plugins/#1/name (="glob")
/plugins/#1/config/set/#0
/plugins/#1/config/set/#1
/plugins/#1/config/set/#2
/plugins/#1/config/set/#3
/plugins/#1/config/set/#4/flags
/plugins/#2/name (="hosts")
/plugins/#3/name (="sync")
/plugins/#4/name (="error")
/plugins/#5/name (="network")
/plugins/#6/name (="backend")

/backend (="#6")

# Part 2: Specific to this plugin

# define relative path the resolver should resolve
/path (="myhosts")

# configuration of get positions
/positions/get/resolver = (="#0")
/positions/get/storage = (="#2")
/positions/get/poststorage/#0 (="#1")

# configuration of set positions
/positions/set/resolver = (="#0")
/positions/set/prestorage/#0 = (="#1")
/positions/set/prestorage/#1 = (="#4")
/positions/set/prestorage/#2 = (="#5")
/positions/set/storage = (="#2")
/positions/set/precommit/#0 = (="#3")
/positions/set/commit = (="#0")
/positions/set/rollback (="#0")
```
