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

To take the changed structure of the plugin arrays into account, the mountpoint configuration needs to be modified accordingly. An example
of the new configuration:

```
system/elektra/mountpoints/\/hosts
system/elektra/mountpoints/\/hosts/config
system/elektra/mountpoints/\/hosts/config/glob/set/#0
system/elektra/mountpoints/\/hosts/config/glob/set/#1
system/elektra/mountpoints/\/hosts/config/glob/set/#2
system/elektra/mountpoints/\/hosts/config/glob/set/#3
system/elektra/mountpoints/\/hosts/config/glob/set/#4
system/elektra/mountpoints/\/hosts/config/glob/set/#4/flags
system/elektra/mountpoints/\/hosts/config/mountpoint
system/elektra/mountpoints/\/hosts/config/path
system/elektra/mountpoints/\/hosts/error
system/elektra/mountpoints/\/hosts/error/rollback
system/elektra/mountpoints/\/hosts/error/rollback/#0
system/elektra/mountpoints/\/hosts/error/rollback/#0/label (="resolver")
system/elektra/mountpoints/\/hosts/error/rollback/#0/name (="resolver_fm_hpu_b")
system/elektra/mountpoints/\/hosts/get
system/elektra/mountpoints/\/hosts/get/poststorage
system/elektra/mountpoints/\/hosts/get/poststorage/#0
system/elektra/mountpoints/\/hosts/get/poststorage/#0/label (="glob")
system/elektra/mountpoints/\/hosts/get/poststorage/#0/name (="glob")
system/elektra/mountpoints/\/hosts/get/resolver
system/elektra/mountpoints/\/hosts/get/resolver/#0
system/elektra/mountpoints/\/hosts/get/resolver/#0/reference (="resolver")
system/elektra/mountpoints/\/hosts/get/storage
system/elektra/mountpoints/\/hosts/get/storage/#0
system/elektra/mountpoints/\/hosts/get/storage/#0/label (="hosts")
system/elektra/mountpoints/\/hosts/get/storage/#0/name (="hosts")
system/elektra/mountpoints/\/hosts/set
system/elektra/mountpoints/\/hosts/set/commit
system/elektra/mountpoints/\/hosts/set/commit/#0
system/elektra/mountpoints/\/hosts/set/commit/#0/reference (="resolver")
system/elektra/mountpoints/\/hosts/set/precommit
system/elektra/mountpoints/\/hosts/set/precommit/#0
system/elektra/mountpoints/\/hosts/set/precommit/#0/label (="sync")
system/elektra/mountpoints/\/hosts/set/precommit/#0/name (="sync")
system/elektra/mountpoints/\/hosts/set/prestorage
system/elektra/mountpoints/\/hosts/set/prestorage/#0
system/elektra/mountpoints/\/hosts/set/prestorage/#0/reference (="glob")
system/elektra/mountpoints/\/hosts/set/prestorage/#1
system/elektra/mountpoints/\/hosts/set/prestorage/#1/label (="error")
system/elektra/mountpoints/\/hosts/set/prestorage/#1/name (="error")
system/elektra/mountpoints/\/hosts/set/prestorage/#2
system/elektra/mountpoints/\/hosts/set/prestorage/#2/label (="network")
system/elektra/mountpoints/\/hosts/set/prestorage/#2/name (="network")
system/elektra/mountpoints/\/hosts/set/resolver
system/elektra/mountpoints/\/hosts/set/resolver/#0
system/elektra/mountpoints/\/hosts/set/resolver/#0/reference (="resolver")
system/elektra/mountpoints/\/hosts/set/storage
system/elektra/mountpoints/\/hosts/set/storage/#0
system/elektra/mountpoints/\/hosts/set/storage/#0/reference (="hosts")
```

The following changes have been made:

- The mountpoint has been moved from `system/elektra/mountpoints/backendname/mountpoint`
to `system/elektra/mountpoints/backendname/config/mountpoint`. That way, the mountpoint of 
the backend can still be read out in the core from the backend plugin's plugin configuration.
- Plugin roles are no longer displayed as array slots, but actually by their names. In addition,
the names of the roles and the plugin arrays have been shortened for redundancy. For example,
`system/elektra/mountpoints/\/hosts/getplugins/#0` is now
`system/elektra/mountpoints/\/hosts/get/resolver`. 
- Each plugin role consists of a linked list containing the plugins fulfilling this role. In the 
configuration, the position of the plugin in the linked list is shown using an array. For example, 
the key `system/elektra/mountpoints/\/hosts/set/prestorage/#1` means that the plugin (in this 
case `error`) belongs to the second position of the linked list belonging to the `prestorage` role
in the `set` array.
- The name, reference name and the label of a plugin are now stored in separate keys to avoid using 
the `#` symbol for something other than arrays. An example from the `error` plugin:

```
system/elektra/mountpoints/\/hosts/set/prestorage/#1
system/elektra/mountpoints/\/hosts/set/prestorage/#1/label (="error")
system/elektra/mountpoints/\/hosts/set/prestorage/#1/name (="error")
```
That way, the `error` plugin is opened and stored for later use with the defined label. If it were to 
be used later, it would be referenced by adding `reference` instead of `label` and `name`.

Another change that had to be made is adding the `modules` KeySet to the `Plugin` structure so that it
can be accessed from within the `backend` plugin.

## Related Decisions

This decision builds upon the development of `kdbCommit()`, discussed in:

https://issues.libelektra.org/2798

## Notes

https://issues.libelektra.org/2963