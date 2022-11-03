# Hooks in KDB

## Problem

Some components of `kdbGet`/`kdbSet` should be optional.
We use the plugin system for that.
However, some of these cases cannot be tied to a mountpoint.
This was the idea of global plugins, but that idea proved problematic.

In the old global plugins implementation:

- Notification does not happen once after final commit, but for every
  plugin
- Problems in spec plugin

These problems can be traced back to the placement of the plugins.
We need to clean up and simplify the placement.

## Constraints

- Plugin interface should be the same. Many plugins, where appropriate, e.g. dbus, should work
  as global plugins w/o any change in code (i.e. only changes
  in contract)

- Global plugins might depend on specific applications or specific
  mount points (it should be possible to enforce global plugins for specific
  applications).

## Assumptions

- Elektra is useful with following types of plugins:
  - mmap
  - spec
  - gopts
  - receiving of notifications (internalnotification)
  - sending of notifications (dbus, ...)
  - recording of changes
- There are not too many types of global plugins, not more than 10

## Considered Alternatives

- generic placements like /prerollback /rollback /postrollback /getresolver
  /pregetcache /pregetstorage /getstorage /postgetstorage /postgetcache
  /setresolver /presetstorage /setstorage /precommit /commit /postcommit
  which can be executed at:
  /init /deinit /foreach
  proved to be too complicated and untestable.

## Decision

Have hooks and API specific to the list of global plugins in assumptions.
These hooks are not shared, so no `list` plugin is needed.

Installed plugins will be used.

In the beginning, we'll hard code the names of the plugins. For changing those plugins symlinks will have to be used.

## Rationale

- allows adding more types of plugins later, also post-1.0
- much clearer semantics for each type, no need to compromise
- easier to implement

## Implications

- remove `global-mount` command
- command for mmap/notification/... enable disable (like current `kdb cache` tool)
- remove `list` plugin
- remove plugins that stop working or disallow global positioning for them
- call `spec` as needed several times
- remove current global plugins mechanism

## Related Decisions

- [Array](array.md)
- [Ensure](../5_implemented/ensure.md)

## Notes
