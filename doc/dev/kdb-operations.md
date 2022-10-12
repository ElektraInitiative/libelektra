# KDB Operations

There are four main _operations_ in `libelektra-kdb`: `open`, `get`, `set` and `close`.
For each of these there is a `kdb*` function the user calls to trigger the operation and plugins export a function for each of the operations they support (at least `get`).

<!-- TODO [new_backend]: Decide whether the extra commit/error functions make sense.
      Possible options are:
      1. keep the separate functions
      2. merge them into set, and let backend plugins dispatch based on elektraPluginGetPhase
      3. in addition to merging into set, add an extra `ElektraGetPhase phase`/`ElektraSetPhase phase` argument to get/set so it is clear that the phase must be taken into account
 -->

Additionally, plugins may implement `commit` and `error`.
These are part of the `set` operation and there is no corresponding `kdbCommit` or `kdbError` function available in `libelektra-kdb`.

The operations `get` and `set` also have different _phases_:

- The `get` operation has: `init`, `resolver`, `cachecheck`, `prestorage`, `storage` and `poststorage`.
- The `set` operation has: `resolver`, `prestorage`, `storage` and `poststorage` followed by `precommit`, `commit` and `postcommit` if the previous phases where successful or by `prerollback`, `rollback` and `postrollback` if the previous phases failed.

These phases are implemented by a backend plugin.
Read the [Documentation on Backend Plugins](./backend-plugins.md) for more information on what backend plugins do.

> **Note** The steps of the operations described below, are referenced in the source code with `// Step X` comments.

## `open` Operation

The `open` operation implemented in `kdbOpen` is the first thing that happens to all `KDB` instances.

The basic flow of this operation is:

1. Create empty `KDB` instance
2. Configure `KDB` instance for bootstrap
3. Run bootstrap `get` operation:
   This loads the contents of `system:/elektra/mountpoints` so that the mountpoints can be configured.
4. Process contract and set up plugins for hooks (see [Hooks](hooks.md))
5. Parse mountpoints:
   This transforms the contents of `system:/elektra/mountpoints` into the internal state stored in a `KDB` instance.
6. Reconfigure `KDB` with real mountpoints:
   This switches the `KDB` instance from bootstrap mode to use the real mountpoint state created above.
7. Add hard coded mountpoints to `KDB` instance:
   There are a few hard coded mountpoints (root mountpoints, `system:/elektra/modules`, `system:/elektra/version`, etc.) that are always present.
   They are added in this step.

Namespaces in mountpoint configs:

- `dir:/`, `user:/` and `system:/` mountpoints can be created without restrictions, except for the reserved sections listed below.
- `spec:/` mountpoints can be created with the same restrictions, but they are also treated specially during `get` and `set`.
- `proc:/` mountpoints are always read-only and receive special treatment during `get`
- `default:/` mountpoints are read-only and receive special treatment during `get`, specifically they only go through the `poststorage` phase
- mountpoints in all other namespaces are entirely illegal

> **Note** The special treatments of the various namespaces are explained below in the sections for the `get` and `set` operation.

Reserved sections:

- Creating a mountpoint for `/elektra` or below in _any namespace_ is forbidden.
  This section of the KDB is reserved for Elektra's own config.
- `system:/elektra/mountpoints`, `user:/elektra/mountpoints` and `dir:/elektra/mountpoints` are all required for the bootstrap process and use a hard coded backend.
  The backends are implemented by a standard file-based backend plugin that is defined at compile-time of `libelektra-kdb`.
- `system:/elektra/version` and `system:/elektra/modules` will always use hard coded read-only backends containing information about this Elektra installation.
  The backends are implemented by special purpose backend plugins.

## `get` Operation

The purpose of the `get` operation is to read data stored in backends into a `KDB` instance.

> **Note:** Some details of a `get` operation are defined in the [contract with backend plugins](./backend-plugins.md).

Properties of `kdbGet()`:

- After calling `kdbGet (kdb, ks, parentKey)`, the KeySet `ks` will contain _all keys_ (including their values) that are stored in _any backend_ with a mountpoint that is _below `parentKey`_.
- After calling `kdbGet (kdb, ks, parentKey)`, below `parentKey` the KeySet `ks` will _mostly_ contain keys that are stored in a backend.
  The exception here are `proc:/` and `spec:/` keys.
  For other namespaces, all keys below `parentKey` will be removed from `ks`.
  For `proc:/` and `spec:/` only keys that overlap with a backend that was loaded will be removed from `ks`.
- The KeySet `ks` _may_ contain other keys not below `parentKey`:
  1. Keys that are not below `parentKey`, but are stored in a backend that contains other keys which are below `parentKey`.
     These keys are returned, because backends are treated as one atomic unit.
     Either all keys within a backend are read, or none of them are.
  2. Keys that were already present in `ks` when `kdbGet()` was called and do not conflict with the goal of representing the current state of the KDB below `parentKey`.
- After calling `kdbGet (kdb, ks, parentKey)`, the Key `parentKey` will only have the `meta:/error/*` or `meta:/warnings/#/*` metakeys, if the errors/warnings originate from this `kdbGet()` call.
  In other words, `kdbGet ()` first clears any existing errors/warnings and only then starts doing the actual work.
- It is an error to use a `parentKey` with a namespace other than: `default:/`, `proc:/`, `spec:`, `dir:/`, `user:/`, `system:/` or cascading

To the caller it looks as if `kdbGet()` had removed all keys below `parentKey`, as well as some others, from `ks` and then loaded the data from the backends.
Which backends are actually read is an implementation detail.
Which keys are removed from `ks` depends on the backends that are read.

`kdbGet()` will always try to be efficient in achieving its goal of reading the keys below `parentKey`.
It is only guaranteed that below `parentKey` the KeySet `ks` correctly represents the state of the KDB.
For the rest of `ks` there are no such guarantees.

> **Note:** In the list below "phase" always refers to a phase of the `get` operation as described in [the backend plugin contract](backend-plugins.md).

The flow of this operation is:

1. Determine the backends needed to read all keys below `parentKey`
2. Run the `open` operation for all required backends that haven't been opened
3. Run the `init` phase on all the backends that haven't been initialized
4. Run the `resolver` phase on all backends
5. From now on ignore all backends, which indicated that there is no update.
6. If all backends are now ignored, **return**.
7. If a global cache plugin is enabled:
   Ask the global cache plugin for the cache handles (normally modification times) for all backends.
8. If all backends have an existing cache entry:
   Run the `cachecheck` phase on all backends
9. If all backends indicated the cache is still valid:
   Ask the global cache plugin for the cached data and **return**.
10. Run the `prestorage` and `storage` phase on all backends.
11. Run the `poststorage` phase of all `spec:/` backends.
12. Merge the data from all backends
13. If enabled, run the `gopts/get` hook.
14. Run the `spec/copy` hook.
15. Split data back into individual backends.
16. Run the `poststorage` phase for all non-`spec:/` backends.
17. Remove all keys which are below the parent key of any backend that has been read from `ks`.
18. Merge the data from all backends into `ks`.
19. If a global cache plugin is enabled, update cache.
20. Run the `notification/send` hook.
    Then **return**.

> **Note:** In case of error, we abort immediately, restore `ks` to its original state and return.

Influence of namespaces:

- `spec:/` backends go through `init`, `resolver`, `cache`, `presetstorage` and `storage` phases as normal, but their `poststorage` phase is called earlier.
  This is required, because any validation and post-processing of `spec:/` keys needs to happen, before they are used as the specification for other keys in the actual `poststorage` phase.
- `dir:/`, `user:/` and `system:/` go through all phases as described above.
- `proc:/` mountpoints go through all the phases as described above, but they are not stored in the cache.
- `default:/` backends only go through the `poststorage` phase.
  This is because `default:/` keys are generated from the specification (stored as `spec:/` keys).
  Therefore, no `default:/` keys can exist before the specification is processed by the `spec/copy` hook.
- keys with other namespaces are always illegal in `ks` (should be enforced via different `KeySet` types)

## `set` Operation

The purpose of the `set` operation is to write data from a `KDB` instance into backends.

> **Note:** Some details of a `set` operation are defined in the [contract with backend plugins](./backend-plugins.md).

Properties of `kdbSet()`:

- When calling `kdbSet (kdb, ks, parentKey)` the contents (key names, values and metadata) of `ks` will _mostly_ not be modified.
  The only modifications that are made to `ks` are those that originate from the `spec/copy` hook.
- _All keys_ in `ks` that are below `parentKey` will be persisted in the KDB, when a `kdbSet (kdb, ks, parentKey)` call returns successfully.
  Additionally, any key in `ks` that shares a backend with another key which is below `parentKey` will also be persisted.
- Calling `kdbSet` may result in an error, if `kdbGet` wasn't called on this `KDB` instance with the same `parentKey` at least once.
- After calling `kdbSet (kdb, ks, parentKey)`, the Key `parentKey` will only have the `meta:/error/*` or `meta:/warnings/#/*` metakeys, if the errors/warnings originate from this `kdbSet()` call.
  In other words, `kdbSet ()` first clears any existing errors/warnings and only then starts doing the actual work.
- It is an error to use a `parentKey` with a namespace other than: `default:/`, `proc:/`, `spec:`, `dir:/`, `user:/`, `system:/` or cascading

The flow of this operation is:

1. Determine the backends needed to write all keys below `parentKey`.
2. Check that all backends are opened and initialized (i.e. `kdbGet()` was called).
3. Run the `spec/copy` hook on `ks` (to add metakeys to newly created keys).
4. Deep-Copy `ks` (below `parentKey`) into a new KeySet `set_ks`
5. Split `set_ks` into individual backends
6. Determine which backends contain changed data.
   Any backend that contains a key that needs sync (via `KEY_FLAG_SYNC`) could contain changed data.
   From now on ignore all backends that have not changed.
   From now on also ignore all backends that were initialized as read-only.
   Issue a warning, if a change was detected (via `KEY_FLAG_SYNC`) in a read-only backend.
   > **Note**: Steps 4-6 might be combined into a single procedure that deep-copies only keys from changed backends into separate KeySets per backend
7. Run the `resolver` and `prestorage` on all backends (abort immediately on error and go to e).
8. Merge the results into a new version of `set_ks`.
9. Run the `spec/remove` hook on `set_ks` (to remove copied metakeys).
10. Split `set_ks` into individual backends again.
11. Run the `storage` and `poststorage` phases on all backends (abort immediately on error and go to e).
12. If everything was successful:
    Run the `precommit` and `commit` phases on all backends (abort immediately on error and go to e), then run the `postcommit` phase on all backends (record all errors as warnings and ignore them) and **return**.

<ol type="a" start="5">
<li>
 If there was an error:
    Run the <code>prerollback</code>, <code>rollback</code> and <code>postrollback</code> phases on all backends and <b>return</b>.
</li>
</ol>

Influence of namespaces:

- `default:/` and `proc:/` keys are completely ignored by `kdbSet()`
- `spec:/`, `dir:/`, `user:/` and `system:/` go through all phases as described above.
- keys with other namespaces are always illegal in `ks` (should be enforced via different KeySet types)

## `close` Operation

The `close` operation is very simple.
It simply frees up all resources used by a `KDB` instance.
