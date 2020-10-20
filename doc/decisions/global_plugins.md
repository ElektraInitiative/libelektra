# Global Plugins

## Problem

- Notification does not happen once after final commit, but for every
  plugin
- Problems in spec plugin

## Constraints

- Plugin interface should be the same. Many plugins, e.g. dbus, should work
  as global plugins w/o any change in code (i.e. only changes
  in contract)
- Global plugins might depend on specific applications or specific
  mount points (it should be possible to enforce global plugins for specific
  applications).

## Assumptions

- Elektra is useful with only 3 types of global plugins:
  - mmap
  - spec
  - notification
- There are not too many types of global plugins, not more than 10

## Considered Alternatives

- generic placements like /prerollback /rollback /postrollback /getresolver
  /pregetcache /pregetstorage /getstorage /postgetstorage /postgetcache
  /setresolver /presetstorage /setstorage /precommit /commit /postcommit
  which can be executed at:
  /init /deinit /foreach
  proved to be too complicated and untestable.

## Decision

Have hooks specific to mmap, spec and notification.
These hooks are not shared, so no `list` plugin is needed.

## Rationale

- allows adding more types of plugins later, also post-1.0
- much clearer semantics for each type, no need to compromise
- easier to implement

## Implications

- remove `list` plugin
- remove plugins that stop working, like internalnotification, or disallow global positioning for them

## Related Decisions

## Notes
