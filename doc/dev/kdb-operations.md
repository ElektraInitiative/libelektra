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

## `get` Operation

The purpose of the `get` operation is to read data stored in backends into a `KDB` instance.

> **Note:** Some details of a `get` operation are defined in the [contract with backend plugins](./backend-plugins.md).

Properties of `kdbGet()`:

- After calling `kdbGet (kdb, ks, parentKey)` the KeySet `ks` will contain _all keys_ (including their values) that are stored in _any backend_ with a mountpoint that is _below `parentKey`_.
- `ks` may also contain other keys outside these backends, if e.g. `kdbGet` determines that it is faster to read a bigger part of the KDB (e.g. because of a cache).
- _Any key_ present in `ks` after calling `kdbGet (kdb, ks, parentKey)` will _always_ be
  - a key read from the KDB
  - OR an untouched key that was already present in `ks` before calling `kdbGet`
- `kdbGet` never removes keys from `ks`.
  It only adds or replaces keys.

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
   Run the `cache` phase on all backends
8. If all backends indicated the cache is still valid:
   Ask the global cache plugin for the cached data and **return**.
9. Run the `prestorage` and `storage` phase on all backends.
10. Run the `poststorage` phase of all `spec:/` backends.
11. Merge the data from all backend
12. Run the `spec` plugin (to copy metakeys).
13. If enabled, run the `gopts` plugin.
14. Split data back into individual backends.
15. Run the `poststorage` phase for all backends.
16. Merge the data from all backends and **return**.

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
4. Run the `spec` plugin (to remove copied metakeys).
5. Split `ks` (below `parentKey`) into individual backends
6. Run the `resolver`, `prestorage`, `storage` and `poststorage` phases on all backends (abort immediately on error, do 7 then go to 9).
7. Run the `spec` plugin (to re-add copied metakeys).
8. If everything was successful:
   Run the `precommit` and `commit` phases on all backends (abort immediately on error and go to 9), then run the `postcommit` phase on all backends.
9. If there was an error:
   Run the `prerollback`, `rollback` and `postrollback` phases on all backends and **return**.
10. Store `ks` in the global cache and **return**.

Influence of namespaces:

- cascading and `meta:/` keys are always illegal in `ks` (should be enforced via different KeySet types)
- `default:/` and `proc:/` keys are completely ignored by `kdbSet()`
- `spec:/`
- `dir:/`, `user:/` and `system:/` go through all phases as described above.

## `close` Operation

The `close` operation is very simple.
It just frees up all resources used by a `KDB` instance.
