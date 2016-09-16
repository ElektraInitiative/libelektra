# Global Plugins

## Issue

- Checker plugins see only part of the configuration and cannot check
  constraints between keys of different mountpoints
- Notification does not happen once after final commit, but for every
  plugin
- Some plugins are not implementable, e.g. global locks (that lock
  before any other plugin and unlock after any other), but also
  journal plugins that keep track of actions to be done and already
  done

## Constraints

- Use list-plugin in configuration and allow arbitrary number of global plugins
- Plugin interface should be the same. Many plugins, e.g. dbus, should work
  as global plugins w/o any change in code (i.e. only changes
  in contract)
- Global plugins might depend on specific applications or specific
  mountpoints (it should be possible to enforce global plugins for specific
  applications).

## Assumptions

- we can enumerate all positions that are useful for global plugins

## Considered Alternatives

- using different plugin interface (like hooks)

## Decision

### Positions

Configuration will be in arrays below the keys:

    system/elektra/globalplugins
                                 /prerollback
                                 /rollback
                                 /postrollback
                                 /getresolver
                                 /pregetstorage
                                 /getstorage
                                 /postgetstorage
                                 /setresolver
                                 /presetstorage
                                 /setstorage
                                 /precommit
                                 /commit
                                 /postcommit

Additionally, below every of these position following subpositions
exist:

                                            /init
                                            /deinit
                                            /foreach

With different semantics each:

- `init` is always paired with `deinit` and can be used for locking purposes.
  It is guaranteed that `deinit` will be called, if `init` was called before.
- `foreach` will be called for every single mountpoint.
- `max once` (without any subposition) will be called maximum once per `kdbGet()/kdbSet()`
  outside the loop. It must be called after `init`, and before `deinit`.


### Return values

If a global plugin returns:

- `-1`: `kdbGet()/kdbSet()` will return with an error
- `0`: `kdbGet()/kdbSet()` will be aborted without an error
  (except `deinit` and `rollback` plugins are executed)
- `1`: `kdbGet()/kdbSet()` will continue as if no hook was
  executed


### Detection within plugins

So that plugins know in which position they currently are, the name of the position
will be written as string in parentkey (not starting with slash to distinguish with
file names).

Exception: `foreach` plugins only get filenames, so to know in which foreach
loop you are, you need to add additional `once` placements to correctly track your
state.


### Contract

Next to positioning information
plugins have to state in their contract that they will work as global plugin, i.e.
do not need to work on individual config files, when following contract
is present:

    infos/status global



### Application-Specific global plugins

If you need a global plugin for your application `kdbAddGlobalPlugin`
from libtools can be used. If the global plugin is already present,
it should be a NOP. For this functionality #684 is needed.

Use cases:

- internal notification within program
- if applications depend on a global plugin to be present, e.g. #689




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

Now called `semlock`-plugin.


### Shell plugins

Run shell code at end of all plugins, e.g. especially doing

    git add
    git commit


### Inference plugins

The globbing would be more natural (derived from specification).
Or even more advanced ways to copy information from specification to the keys, e.g. type inference

Now called `spec`-plugin.

### Journalling plugins

It should be possible to write plugins which need all file names of all resolver plugins.
E.g. journalling, global mmap.

For mmap it could work the following way:

        getresolver/after/foreach

is responsible to check if all files resolved are still the same file (and same number of files),
and if the `mtime` of the mmap file is newer than the resolved file.
Iff this is the case for every mountpoint we will (try) to load the mmaped file in:

        getresolver/after/once

The loading of the mmap might fail:

- some checksums missing/wrong (file was tampered with)
- endianness different
- size of types different

If the loading failed, we will continue by returning 1,
if the loading was successful we prematurely abort `kdbGet` by returning 0.

If we continued with `kdbGet` we want to persist the KeySet for
the next `kdbGet()` with the same parameters using the global hook:

        getresolver/after/once




## Implications

### Default global plugins

Its useful to have some important global plugins, e.g. locking by default.
See #690.

Internal list to be used when no system/elektra/global_mountpoints/ exists.

State diagrams of plugins need to be redrawn to also include global plugin
states.

## Related decisions

## Notes

### Open Points

- How to test global plugins? (Test plugin that mounts itself on every position and checks if it is called correctly?)

## Implementation Hints

- add `Plugin *globalPlugins [NR_OF_PLUGINS]` to `_KDB`
- during `kdbOpen`, `system/elektra/globalplugins/` is read and plugins are constructed and placed into `globalPlugins`.
- In kdbGet and kdbSet hooks execute one of these plugins
- by default
 - the plugins are all the same `list` plugins, and their subplugins are executed, when `system/elektra/globalplugins/_` states they should be executed
 - a `lock` plugin that executes at begin and end of kdbGet and kdbSet, respective, i.e.  postrollback preget postget preset postcommit
 - the `lock` plugin contains the code currently found in resolver

