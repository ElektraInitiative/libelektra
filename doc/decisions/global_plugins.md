# Global Plugins

## Issue

- Checker plugins see only part of the configuration and cannot check
  constraints between keys of different mountpoints
- Notification does not happen once after final commit, but for every
  plugin
- Some plugins are not implementable, e.g. global locks (that lock
  before any other plugin and unlock after any other), but also
  journal plugins that keep track of actions to be done and already
  done (latter is maybe out of scope for this decision)

## Constraints

- Use arrays in configuration and allow arbitrary number of global plugins
- Plugin interface should be the same. Many plugins, e.g. dbus, should work
  as global plugins w/o any significant change (e.g. only one more entry
  in contract)

## Assumptions

- Global plugins to not depend on specific applications nor specific
  mountpoints.
- There is no plugin that works as global plugin but will not work
  as mounted plugin, too (to investigate: locker plugins can lead to deadlocks?)

## Considered Alternatives

- using different plugin interface (like hooks)

## Decision

Configuration will be in arrays below the keys:

    system/elektra/globalplugins/
        prerollback
        postrollback
        preget
        postget
        preset
        setstorage
        precommit
        postcommit

Plugins state in contract that they will work as global plugin, i.e.
do not need to work on individual config files, when following contract
is present:

    infos/global

The `kdb`-tool should have following list-like interface:

    kdb global-plugin-add
    kdb global-plugin-del
    kdb global-plugin-list


## Argument

Some nice features that will be implemented as global plugins.

### Transformation

Transformation keys which are read and transformed to be usable by the application:

    [dir/a]
    transform=/x
    transform/python=...upper()
             /lua=..

(actually two plugins are involved: one that fetches transformation keys, the other
 that executes the transformation code)


- preget: fetch all foreign keys (kdbGet)
- postget: run transformation for all foreign keys


### Global lock

simplifies threading and process locking by not having to think about
recursive cases.


### Shell plugins

Run shell code at end of all plugins, e.g. especially doing

    git add
    git commit


### Inference plugins

The globbing would be more natural (derived from specification).
Or even more advanced ways to copy information from specification to the keys, e.g. type inference


### Journalling plugins

It should be possible to write plugins which need all file names of all resolver plugins.
E.g. journalling, global mmap.


## Implications

### Default global plugins

Its useful to have some important global plugins, e.g. locking by default.
Internal list to be used when no system/elektra/global_mountpoints/ exists.

State diagrams of plugins need to be redrawn to also include global plugin
states.

## Related decisions

## Notes

### Open Points

- How to test global plugins?
- locker plugins can lead to deadlocks? (must be avoided by contract?)

## Implementation Hints

- add `Plugin *globalPlugins [NR_OF_PLUGINS]` to `_KDB`
- during `kdbOpen`, `system/elektra/globalplugins/` is read and plugins are constructed and placed into `globalPlugins`.
- In kdbGet and kdbSet hooks execute one of these plugins
- by default
 - the plugins are all the same `list` plugins, and their subplugins are executed, when `system/elektra/globalplugins/_` states they should be executed
 - a `lock` plugin that executes at begin and end of kdbGet and kdbSet, respective, i.e.  postrollback preget postget preset postcommit
 - the `lock` plugin contains the code currently found in resolver

