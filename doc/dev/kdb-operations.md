# Proposal: KDB Operations

There are four main _operations_ in `libelektra-kdb`: `open`, `get`, `set` and `close`.
For each of these there is a `kdb*` function the user calls to trigger the operation and plugins implement a `elektra<Plugin>*` function for each of the operations they support (at least `get`).

Additionally, plugins may implement `elektra<Plugin>Commit` and `elektra<Plugin>Error`.
These are part of the `set` operation and there is no corresponding `kdbCommit` or `kdbError` function available in `libelektra-kdb`.

The operations `get` and `set` also have different _phases_:

- The `get` operation has: `init`, `resolver`, `cachecheck`, `prestorage`, `storage` and `poststorage`.
- The `set` operation has: `resolver`, `prestorage`, `storage` and `poststorage` followed by `precommit`, `commit` and `postcommit` if the previous phases where successful or by `prerollback`, `rollback` and `postrollback` if the previous phases failed.

These phases are implemented by a backend plugin.
Read the [Documentation on Backend Plugins](./backend-plugins.md) for more information on what backend plugins do.

## `open` Operation

The `open` operation implemented in `kdbOpen` is the first thing that happens to all `KDB` instances.

The basic flow of this operation is:

1. Create empty `KDB` instance
2. Configure `KDB` instance for bootstrap
3. Run bootstrap `get` operation
4. Mount global plugins
5. Process contract
6. Parse mountpoints
7. Configure `KDB` instance with real mountpoints
8. Add hardcoded mountpoints to `KDB` instance

Namespaces in mountpoint configs:

- cascading mountpoints act as an alias for identical `proc:/`, `dir:/`, `user:/`, `system:/` and `default:/` mountpoints.
- `meta:/` mountpoints are illegal
- explicit `default:/` mountpoints are illegal
  They can only exist via cascading mountpoints.
  This is so that default values can be checked and validated by plugins.
- `spec:/` mountpoints may exist, but they are treated differently during `get` and `set`
- `proc:/` mountpoints may exist, but they are read-only (see below)
- `dir:/`, `user:/` and `system:/` mountpoints can be created without restrictions

Other restrictions:

- Creating mountpoints for root keys (`system:/`, `user:/`, etc.) is not allowed.
  These parts of the KDB always use hardcoded backends that can only be configured at compile-time.
- Creating a mountpoint for `system:/elektra` or any mountpoint below is not allowed.
  This part of the KDB will always be stored in the bootstrap backend.
  While the only part that is actually required for the bootstrap process is the one below `system:/elektra/mountpoints`, everything below `system:/elektra` is reserved and should only be used internally.
  Additionally, `system:/elektra/version` and `system:/elektra/modules` will always point to hardcoded read-only backends containing information about this Elektra installation.

## `get` Operation

The purpose of the `get` operation is to read data stored in backends into a `KDB` instance.

> **Note:** Some details of a `get` operation are defined in the [contract with backend plugins](./backend-plugins.md).

Properties of `kdbGet()`:

- After calling `kdbGet (kdb, ks, parentKey)`, the KeySet `ks` will contain _all keys_ (including their values) that are stored in _any backend_ with a mountpoint that is _below `parentKey`_.
- After calling `kdbGet (kdb, ks, parentKey)`, below `parentKey` the KeySet `ks` will _only_ contain keys that are stored in a backend.
- The KeySet `ks` may contain other keys not below `parentKey`.
  These keys fall into one of two categories:
  1. Keys that are stored in a backend that is not below `parentKey`.
     `kdbGet()` may decide that it is more efficient (e.g. because of a cache) to return more keys than requested.
  2. Keys that were already present in `ks` when `kdbGet()` was called.

In simpler terms, this means to the caller it looks as if `kdbGet()` had removed all keys below `parentKey` from `ks` and then loaded the data from the backends.
Below `parentKey` the KeySet `ks` correctly represents the state of the KDB.
For the rest of `ks` there are no such guarantees.

TODO: cache correct? notifications? (just call at the end?)

The basic flow of this operation is:

1. Determine the backends needed to read all keys below `parentKey`
2. Run the `init` phase on all the backends that haven't been initialized
3. Run the `resolver` phase on all backends
4. From now on ignore all backends, which indicated that there is no update.
5. If all backends are now ignored, **return**.
6. If a global cache plugin is enabled:
   Ask the global cache plugin for the cache entry IDs for all backends.
7. If all backends have an existing cache entry:
   Run the `cachecheck` phase on all backends
8. If all backends indicated the cache is still valid:
   Ask the global cache plugin for the cached data and **return**.
9. Run the `prestorage` and `storage` phase on all backends.
10. Run the `poststorage` phase of all `spec:/` backends.
11. Merge the data from all backends
12. If enabled, run the `gopts` plugin.
13. Run the `spec` plugin (to copy metakeys).
14. Split data back into individual backends.
15. Run the `poststorage` phase for all backends.
16. Merge the data from all backends.
17. Update cache and **return**.

> **Note:** In case of error, we abort immediately, restore `ks` to its original state and return.

Influence of namespaces:

- cascading and `meta:/` keys are always illegal in `ks` (should be enforced via different KeySet types)
- `spec:/` backends only go through `init`, `resolver`, `cache`, `presetstorage` and `storage` phases as normal, but their `poststorage` phase is called earlier.
- `proc:/`, `dir:/`, `user:/` and `system:/` go through all phases as described above.
- `default:/` backends only go through the `poststorage` phase.

## `set` Operation

The purpose of the `set` operation is to write data from a `KDB` instance into backends.

> **Note:** Some details of a `set` operation are defined in the [contract with backend plugins](./backend-plugins.md).

Properties of `kdbSet()`:

- When calling `kdbSet (kdb, ks, parentKey)` the contents (key names, values and metadata) of `ks` _will not be modified_.
- _All keys_ in `ks` that are below `parentKey` will be persisted in the KDB, when a `kdbSet (kdb, ks, parentKey)` call returns successfully.
- Calling `kdbSet` results in an error, if `kdbGet` wasn't called on this `KDB` instance at least once.

TODO: cache correct? notifications? (just call at the end?)

The basic flow of this operation is:

1. Check if `ks` needs sync (via `KS_FLAG_SYNC` flag), if so go to 3.
2. Check if any key in `ks` below `parentKey` needs sync (via `KEY_FLAG_SYNC`).
   If neither `ks` nor any of the keys need sync, **return**.
3. Determine the backends needed to write all keys below `parentKey`.
4. Check that all backends are initialized (i.e. `kdbGet()` was called).
5. From now on ignore all backends that were initialized as read-only.
6. Run the `spec` plugin on `ks` (to add metakeys for new keys).
7. Deep-Copy `ks` (below `parentKey`) into a new KeySet `set_ks`
8. Split `set_ks` into individual backends
9. Run the `resolver` and `prestorage` on all backends (abort immediately on error and go to E).
10. Merge the results into a new version of `set_ks`.
11. Run the `spec` plugin on `set_ks` (to remove copied metakeys).
12. Split `set_ks` into individual backends again.
13. Run the `storage` and `poststorage` phases on all backends (abort immediately on error and go to E).
14. If everything was successful:
    Run the `precommit` and `commit` phases on all backends (abort immediately on error and go to E), then run the `postcommit` phase on all backends and **return**.

<ol type="A" start="5">
<li>
 If there was an error:
    Run the <code>coderollback</code>, <code>rollback</code> and <code>postrollback</code> phases on all backends and <b>return</b>.
</li>
</ol>

Influence of namespaces:

- cascading and `meta:/` keys are always illegal in `ks` (should be enforced via different KeySet types)
- `default:/` and `proc:/` keys are completely ignored by `kdbSet()`
- `spec:/`, `dir:/`, `user:/` and `system:/` go through all phases as described above.

## `close` Operation

The `close` operation is very simple.
It just frees up all resources used by a `KDB` instance.
