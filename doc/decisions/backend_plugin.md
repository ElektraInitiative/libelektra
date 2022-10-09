# Backend Plugin

## Problem

- Backends store plugins in arrays which have a fixed number of slots for each plugin role. The number of plugins which can be assigned is limited,
  making it easy to reach the limit if many plugins are in use.
- As structs, backends are separate from the plugin interface and integrated into the core of Elektra. This makes it difficult to perform operations
  such as nesting plugins, or to develop other implementations for backends.

## Constraints

- It should be possible for all existing plugins to run normally

## Assumptions

## Considered Alternatives

- Multiple storage plugins within a single backend
- Plugin containing more plugin slots

## Decision

- The current backend implementation will be redeveloped into a backend plugin. That way, the core of Elektra will only access backends through
  the standard plugin interface.
- The new backend plugin will support an unlimited number in any position where more than one plugin is sensible.
  e.g., unlimited plugins in `poststorage`, but only a single one in `storage`

## Rationale

- Making backends plugins themselves detaches their implementation from the core of Elektra, making it possible to develop new kinds of backends
  without major changes to the core itself.
- As plugins, backends can contain further backends, making it possible to nest plugins and enabling new kinds of plugin combinations such as
  fallback storage options.

## Implications

The structure of `system:/elektra/mountpoints` has to change to accommodate different kinds of backend plugins.

## Related Decisions

## Notes

https://issues.libelektra.org/2963
