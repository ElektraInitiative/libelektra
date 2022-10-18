# Algorithm

You might want to read [about architecture](architecture.md) and [data structures](data-structures.md) first.

## Outdated

<!-- TODO [new_backend]: Update the text below using the docs listed in the warning. -->

> **Warning** Many of the things described below (especially about `KDB` and the `kdb*` functions) are outdated.
> See [`kdb-operations.md`](kdb-operations.md) and [`kdb-contracts.md`](kdb-contracts.md) for up-to-date information.

## Introduction

In this section, we will explain the heart of Elektra. `kdbOpen()` is
responsible for the setup and the construction of the data structures
needed later. `kdbGet()` does, together with the plugins, all actions
necessary to read in the configuration. `kdbSet()` orchestrates the
plugins to write out the configuration correctly. `kdbClose()` finally
frees all previously allocated data structures.

### kdbOpen

`kdbOpen()` retrieves the _mount point configuration_ with
`kdbGet()` using the _default backend_. During this process,
the function sets up the data structures which are needed for later
invocations of `kdbGet()` or `kdbSet()`. All backends are opened and
mounted in the appropriate parts of the key hierarchy. The resulting
backends are added both to the `Split` and the `Trie` object. `kdbOpen()`
finally returns a `KDB` object that contains all this information.

The reading of the mount point configuration and the consequential self
configuring of the system is called _bootstrapping_. Elektra builds
itself up with a default backend (consisting of `libelektra-resolver`
and `libelektra-storage`).
[Read more about bootstrapping here](/doc/help/elektra-bootstrapping.md)

`kdbOpen()` creates a `Split` object. It adds all backend handles and
`parentKeys` during bootstrapping. So the buildup of the `Split` object
takes place once. The resulting object is then used for both `kdbGet()`
and `kdbSet()`. This approach is much better testable because the
`Split` object is first initialised using the mount point configuration --
separated from the filtering of the backends for every specific `kdbGet()`
and `kdbSet()` request.

Afterwards the key hierarchy is static. Every application using Elektra
will build up the same key database. Application-specific mount points
are prohibited because changes of mount points would destroy the global
key database. Elektra could not guarantee that every application
retrieves the same configuration with the same key names any longer.

In `kdbOpen()`, nearly no checks are done regarding the expected
behavior of the backend. The contract checker guarantees that only
appropriate mount points are written into the mount point configuration.
`kdbOpen()` checks only if the opening of plugin was successful. If not,
the backend enclosing the plugin is not mounted at all.

### Removing Keys

In Elektra version 0.6, removing keys was an explicit request. Only a
single `Key` object could be removed from the database. For configuration
files this method is inapplicable. For `filesys`, however, it was easy
to implement.

In Elektra version 0.7, the behavior changed. Removing keys was
integrated into `kdbSet()`. The user tagged keys that should be removed.
After the next `kdbSet()`, these keys were removed from the key database.
On the one hand, backends writing configuration files simply ignored
the keys marked for removal. On the other hand, `filesys` needed
that information to remove the files. To make this approach work for
`filesys`, the marked keys were located at the very end of the `KeySet`
and sorted in reverse. With this trick, recursive removing worked
well. But this approach had major defects in the usage of `KeySet`.
Because marking a key to be removed changed the sort order of the key
set `ksLookupByName()` did not find this key anymore.

So in the present version removing keys is consistent again.
A `KeySet` describes the current configuration. The user can reduce
the `KeySet` object by _popping_ keys out. The `kdbSet()`
function applies exactly this configuration as specified by the key set
to the key database. Contrary to the previous versions, the popped keys
of the key set will be permanently removed.

The new circumstance yields **idempotent**
properties for `kdbSet()`. The same `KeySet` can be applied multiple
times, but after the first time, the key database will not be changed
anymore. Note that `kdbSet()` actually detects that
there are no changes and will do nothing. To actually show the idempotent
behavior the KeySet has to be regenerated or the key database needs to
be reopened.

It is, however, not known if keys should be removed permanently
only by investigating the `KeySet`. But only if this knowledge is
present, the core can decide if the key set needs to be written out or
if the configuration is unchanged. So we decided to track how many keys
are delivered in `kdbGet()`. If the size of the `KeySet` is lower than
this number determined at the previous `kdbGet()`, Elektra’s core knows
that some keys were popped. Hence, the next `kdbSet()` invocation needs
to change the concerned key database.

The situation is now much clearer. The semantics of popping a key will
result in removing the key from the key database. And the intuitive
idea that a `KeySet` will be applied to the key database is correct again.

## kdbGet

It is critical for application startup-time to retrieve the
configuration as fast as possible. Hence, the design goal of the
`kdbGet()` algorithm is to be efficient while still enabling plugins
to have relaxed postconditions. To achieve this, the sequence of
_syscalls_ must be optimal. On the other hand, it is
not tolerable to waste time or memory inside Elektra’s core, especially
during an initial request or when no update is available.

The synopsis of the function is:

```c
int kdbGet(KDB *handle, KeySet *returned, Key * parentKey);
```

The user passes a key set, called `returned`.
If the user invokes `kdbGet()` the first time, he or she will usually
pass an empty key set. If the user wants to update the application's
settings, `returned` will typically contain the configuration of the
previous `kdbGet()` request. The `parentKey` holds the information
below which key the configuration should be retrieved. The `handle`
contains the data structures needed for the algorithm, like the `Split`
and the `Trie` objects.

`kdbGet()` does a rather easy job, because `kdbSet()` already guarantees
that only well formatted, non-corrupted and well-typed configuration is
written out in the key database. The task is to query all backends in
question for their configuration and then merge everything.

### Responsibility

A backend may yield keys that it is not responsible for.
It is not possible for a backend to know that another backend has been
mounted below and the other backend is now responsible for some of the
keys that are still in the storage. Additionally, plugins are not able
to determine if they are responsible for a key or not. Consequently, it
can happen that more than one backend delivers a key with the same name.

`kdbGet()` ensures that a key is uniquely identified by its name.
Elektra’s core will [pop](/doc/help/elektra-glossary.md) keys that are
outside of the backend's responsibility. Hence, these keys will not be
passed to the user and we get the desired behavior: The nearest mounted
backend to the key is responsible.

For example, a generator plugin in the backend (A) always emits
following keys. (A) and (B) indicate from which backend the
key comes from.

```
user:/sw/generator/akey (A)
user:/sw/generator/dir (A)
user:/sw/generator/dir/outside1 (A)
user:/sw/generator/dir/outside2 (A)
```

It will still
return these keys even if the plugin is not responsible for some
of them anymore. This can happen if another backend B is mounted
to `user:/sw/generator/dir`. In the example it yields the
following keys:

```
user:/sw/generator/dir (B)
user:/sw/generator/dir/new (B)
user:/sw/generator/dir/outside1 (B)
user:/sw/generator/outside (B)
```

In this situation `kdbGet()` is responsible to pop all three keys at,
and below, `user:/sw/generator/dir` of backend (A) and the key
`user:/sw/generator/outside` of backend (B). The user will get the
resulting key set:

```
user:/sw/generator/akey (A)
user:/sw/generator/dir (B)
user:/sw/generator/dir/new (B)
user:/sw/generator/dir/outside1 (B)
```

Note that the key exactly at the mount point comes from the backend mounted
at `user:/sw/generator/dir`.

### Sequence

`kdbOpen()` already creates a `Split` object for the whole configuration
tree. In this object, `kdbOpen()` will append a list of all backends
available. A specific `kdbGet()` request usually includes only a part
of the configuration. For example, the user is only interested in
keys below `user:/sw/apps/userapp`. All backends that cannot
contribute to configuration below `user:/sw/apps/userapp` will be
omitted for that request. To achieve this, parts of the `Split` object
are filtered out. After this step we know the list of backends involved.
The `Split` object allocates a key set for each of these backends.

Afterwards the first plugin of each backend is called to determine if
an update is needed. If no update is needed, the algorithm has finished
and returns zero.

Now we know which backends do not need an update. For these backends, the
previous configuration from `returned` is appointed from to the key sets
of the `Split` object. The algorithm will not set the _syncbits_
of the `Split` object for these backends because the storage of the
backends already contains up-to-date configuration.

The other backends will be requested to _retrieve_ their
configuration. The initial empty `KeySet` from the `Split` object and
the relevant file name in the key value of `parentKey` are passed to each
remaining plugin. The plugins extend, validate and process the key set.
When an error has occurred, the algorithm can stop immediately because the
user's `KeySet` `returned` is not changed at this point. When this part
finishes, the `Split` object contains the whole requested configuration
separated in various key sets.

Subsequently the freshly received keys need some _post-processing_:

- Newly allocated keys in Elektra always have the _sync flag_ set.
  Because the plugins allocate and modify keys with the same
  functions as the user, the returned keys will also have their sync flag
  set. But from the user's point of view the configuration is unmodified.
  So some code needs to remove this sync flag. To relax the post conditions
  of the plugins, `kdbGet()` removes it.
- To detect removed keys in subsequent `kdbSet()` calls, `kdbGet()`
  needs to store the number of received keys of each backend.
- Additionally, for every key it is checked if it belongs to this
  backend. This makes sure that every key comes from a single source
  only as designated by the `Trie`. In this process, Elektra pops all
  duplicated and overlapping keys in favor of the responsible backend.

The last step is to _merge_ all these key sets together. This step
changes the configuration visible to the user. After some cleanup the
algorithm finally finishes.

### Updating Configuration

The user can call `kdbGet()` often even if the configuration or parts
of it are already up-to-date. This can happen when applications reread
configuration in some events. Examples are signals (SIGHUP is
the signal used for that on Unix systems. It is sent when the program's
controlling terminal is closed. Daemons do not have a terminal so
the signal is reused for reloading configuration.), notifications,
user requests and in the worst case periodical attempts to reread
configuration.

The given goal is to keep the sequence of needed syscalls low. If no
update is needed, it is sufficient to request the timestamp
(On POSIX systems using `stat()`) of every file. No other syscall
is needed. Elektra’s core alone cannot check that because getting
a timestamp is not defined within the standard C99. So instead the
resolver plugin handles this problem. The resolver plugin returns 0 if
nothing has changed.

This decision yields some advantages. Both the storage plugins and
Elektra’s core can conform to C99. Because the resolver plugin is the
very first in the chain of plugins, it is guaranteed that no useless
work is done.

### Initial kdbGet Problem

Because Elektra provides self-contained configuration, `kdbOpen()`
has to retrieve settings in the _bootstrapping_ process below
`system:/elektra` as explained in `bootstrapping`.
Because of the new way to keep track of removed keys, the internally
executed `kdbGet()` creates a problem. Without countermeasures even
the first `kdbGet()` of a user requesting the configuration below
`system:/elektra` fails, because the resolver finds out that the
configuration is already up-to-date. The configuration delivered by the
user is empty at this point. As a result, the empty configuration will
be appointed and returned to the user.

A simple way to resolve this issue is to reload the default backend after
the internal configuration was fetched. Reloading resets the timestamps
and `kdbGet()` works as expected.

## kdbSet

Not performance, but robust and reliable behavior is the most
important issue for `kdbSet()`. The design was chosen so that some
additional in-memory comparisons are preferred to a suboptimal sequence
of `syscalls`. The algorithm makes sure that keys
are written out only if it is necessary, because applications can call
`kdbSet()` with an unchanged `KeySet`. For the code to decide this,
performance is important.

### Properties

`kdbSet()` guarantees the following properties:

- Modifications to permanent storage are only made when the
  configuration was changed.
- When errors occur, every plugin gets a chance to rollback its
  changes as described in **exception safety**.
- If every plugin does this correctly, the whole `KeySet` is
  propagated to permanent storage. Otherwise nothing is changed in the
  key database. Plugins delivered with Elektra meet this requirement.

The synopsis of the function is:

```c
int kdbSet(KDB *handle, KeySet *returned, Key * parentKey);
```

The user passes the configuration using the `KeySet` `returned`. The key
set will not be changed by `kdbSet()`. The `parentKey` provides a way
to limit which part of the configuration is written out. For example,
the `parentKey` `user:/sw/org/app/#0/current` will induce `kdbSet()` to
only modify the key databases below `user:/sw/org/app` even
if the `KeySet` `returned` also contains more configuration. Note that
all backends with no keys in `returned` but that are below `parentKey`
will completely wipe out their key database. The `KDB` handle contains
the necessary data structures.

### Search for Changes

As a first step, `kdbSet()` _divides_ the configuration passed in
by the user to the key sets in the `Split` object. `kdbSet()` searches
for every key if the _sync flag_ is checked. Then `kdbSet()`
decides if a key was removed from a backend by comparing the actual
size of the key set with the size stored from the last `kdbGet()` call.
We see that it is necessary to call `kdbGet()` first before invocations of
`kdbSet()` are allowed.

We know that data of a backend has to be written out if at least one key
was changed or removed. If no backend has any changes, the algorithm
will terminate at this point. The careful reader notices that the
process involves no file operations.

### Duplicated Key Sets

If some backends need synchronization, the algorithm continues by
filtering out all backends in the `Split` object that do not have changes.
At this point, the `Split` object has a list of backends with their
respective key sets.

Plugins in `kdbSet()` can change values. Other than in `kdbGet()`,
the user is not interested in these changes. Instead, the values are
transformed to be suitable for the storage. To make sure that the
changed values are not passed to the user, the algorithm continues with
a _deep duplication_ of all key sets in the `Split` object.

### Resolver

All plugins of each included backend are executed one by one up to the
resolver plugin. If this succeeds, the resolver plugin is responsible
for committing these changes. After the successful commit, _error
codes_ of plugins are ignored. Only logging and notification
plugins are affected.

### Atomic Replacement

Up to now only file-based storages with atomic properties were
developed. The replacement of a file with another file that has not
yet been written is not trivial. The straightforward way is to lock a
file and start writing to it. But this approach can result in broken or
partially finished files in events like “out of disc space”, signals
or other asynchronous aborts of the program.

A temporary file solves most of this problem, because in problematic events
the original file stays untouched. When the temporary file is written
out properly, it is renamed and the original configuration file is
overwritten. But another concurrent invocation of `kdbSet()` can try to
do the same with the result that one of the newly written files is lost.

To avoid this problem, locks are needed and protect cooperating processes
(such as other processes using Elektra).

Additionally modification time is used to detect if a file was modified.
Unfortunately the modification time on some file systems has
a resolution of one second. So any changes within that time slot
might not be recognized.

### Errors

The plugins within `kdbSet()` can fail for a variety of reasons.
Conflicts occur most frequently. A conflict means that
during executions of `kdbGet()` and `kdbSet()` another program has changed
the key database. In order not to lose any data, `kdbSet()` fails without
doing anything. In conflict situations Elektra leaves the programmer
no choice. The programmer has to retrieve the configuration using
`kdbGet()` again to be up-to-date with the key database. Afterwards it
is up to the application to decide which configuration to use. In this
situation it is the best to ask the user, by showing him the description
and reason of the error, how to continue:

1. Save the configuration again. The changes of the other program
   will be lost in this case.
2. The key database can also be left unchanged as the other program
   wrote it. After using `kdbGet()` the application is already up-to-date
   with the new configuration. All configuration changes the user made
   before will be lost.
3. The application can try to merge the key sets to get the best
   result. If no key is changed on both sides the result is clear, otherwise
   the application has to decide if the own or the other configuration should
   be favored. The result of the merged key sets has to be written out with
   `kdbSet()`.
4. Merging the key sets can be done with `ksAppend()`. The source
   parameter is the preferred configuration. Note that the downside of
   the third option is that the merged configuration might not be valid.

Sometimes a concrete key causes the problem that the whole key set cannot
be stored. That can happen on validation or because of type errors.
Such errors are usually caused by a mistake made by the user. So the
user is responsible for changing the settings to make it valid again.
In such situations, the _internal cursor_ of the `KeySet` `returned`
will point to the problematic key.

A completely different approach is to export the configuration when
`kdbSet()` returned an error code. The user can then edit, change or
merge this configuration with more powerful tools. Finally, the user
can import the configuration into the global key database. The export
and import mechanism is called "streaming" and will be explained in
_streaming_.
