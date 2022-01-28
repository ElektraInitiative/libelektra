# Global Plugins

## Problem

- Notification does not happen once after final commit, but for every
  plugin
- Problems in spec plugin

These problems can be traced back to the placement of the plugins.
We need to clean up and simplify the placement.

## Constraints

- Plugin interface should be the same. Many plugins, e.g. dbus, should work
  as global plugins w/o any change in code (i.e. only changes
  in contract)
- Global plugins might depend on specific applications or specific
  mount points (it should be possible to enforce global plugins for specific
  applications).

## Assumptions

- Elektra is useful with following types of global plugins:
  - mmap
  - spec
  - gopts
  - receiving of notifications (internalnotification)
  - sending of notifications (dbus, ...)
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

## Related Decisions

- [Array](array.md)
- [Ensure](ensure.md)

## Notes
