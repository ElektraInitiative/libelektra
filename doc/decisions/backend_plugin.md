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
- The `getplugins`, `setplugins` and `errorplugins` arrays will be changed into arrays of linked lists. Each time a plugin is added to a specific
slot, it will be added at the end of the linked list.

## Rationale

- Making backends plugins themselves detaches their implementation from the core of Elektra, making it possible to develop new kinds of backends
  without major changes to the core itself.
- As plugins, backends can contain further backends, making it possible to nest plugins and enabling new kinds of plugin combinations such as
  fallback storage options.

## Implications

## Related Decisions

This decision builds upon the development of `kdbCommit()`, discussed in:

https://issues.libelektra.org/2798

## Notes

https://issues.libelektra.org/2963