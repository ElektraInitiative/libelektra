# Plugin Struct

## Problem

`struct _Plugin` contains some unnecessary data.

## Constraints

## Assumptions

## Considered Alternatives

> **Note**: Not all of these are alternatives, most of them could be combined.

1. Replace `KeySet * global` and `KeySet * modules` with `KDB * kdb`.

   This would require changing the `elektraPluginOpen` API to take a `KDB * kdb`, or a `Plugin * existing` from which we extract the `KDB *` (the first plugin could use a stack allocated `Plugin` with just `KDB *` set).

2. Replace all the `kdb*` function pointers and `char * name` with a `KeySet * contract`.

   The contract also contains the function pointers, so this avoids duplication and should reduce memory usage.
   This could potentially affect runtime performance, because instead of having a pointer directly, we need to first do a lookup in a keyset.

3. Remove `size_t refcounter`.

   Plugins are never shared between mountpoints.
   There is also no way of increasing `refcounter` via the API.

4. (Based on all above) Change how `struct _Plugin` is used.

   This is the most comprehensive change, it combines all the above to remove as much duplication as possible.
   It consists of these parts:

   - Create a `struct _PluginData` that will be used in `struct _BackendData` instead of `struct _Plugin`.

     ```c
     struct _PluginData {
        KeySet * config; // config of plugin
        void * data; // private data of plugin
        char * name; // name of the plugin
     };
     ```

     The `name` is needed to get the correct functions from `kdb->contracts` (see below).

   - Change `struct _Plugin` to the following and use stack allocated instances created when calling a plugin function.

     ```c
     struct _Plugin {
       KeySet * config; // copied from struct _PluginData
       void * data; // copied from struct _PluginData
       KDB * kdb; // pointer to KDB instance calling this plugin right now, may be copied from other plugin calling this plugin
     };
     ```

   - Add a `KeySet * contracts` to `struct _KDB`.
     This contains all the contracts for all the plugins currently loaded.

     To call a plugin function you need an existing `Plugin *` from which the `KDB * kdb` will be taken.
     Then the function will be looked up in `kdb->contracts`.
     Inside `libelektra-kdb` the existing `Plugin *` is faked with a stack allocated `struct _Plugin` containing only a `KDB *`.

     > **Note** If the changes to `KeySet * modules` from the ["Plugin Contract Function"](../0_drafts/plugin_contract_function.md) decision are implemented, then `kdb->contracts` becomes `kdb->modules`.

5. Introduce a new `KeySet * shared` for sharing data among plugins of a single backend.

   This is similar to `KeySet * global`, but not shared between different mountpoints.
   The `KeySet * shared` would be a copy of the one in `struct _BackendData` (also new field), which owns the `KeySet`.

   This solves the issue that `libelektra-kdb` must currently use `KeySet * global` to communicate extra data like the plugins belonging to a backend (for access in backend plugins).

   When combined with the stack allocated `struct _Plugin` from option 4, this would not affect memory usage as much as otherwise.
   In that case, it would only be one `struct _KeySet` per backend, when otherwise we would also add one pointer for every loaded plugin instance.

## Decision

Don't do anything for 1.0.

## Rationale

## Implications

## Related Decisions

- [Plugin Contract Function](../0_drafts/plugin_contract_function.md)
- [Commit Function](../0_drafts/commit_function.md)

## Notes
