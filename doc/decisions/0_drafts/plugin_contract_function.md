# Plugin Contract Function

## Problem

Contract for a plugin is currently retrieved by calling the plugin's `get` function with a special `parentKey`.
This makes the `get` function awkward to write.
You must always first check for the special `parentKey`.
It also makes the `get` function mandatory, even if the plugin doesn't implement any real `get` functionality.

Additionally, the actual public API exported from shared library to make it a plugin is the `ELEKTRA_PLUGIN_EXPORT` function.
This function always calls `elektraPluginExport`, which allocates and half initializes a `struct _Plugin`.
Specifically, only the `name` field and the `kdb*` function pointers are set.
This same information is also provided via the contract.
The function pointers are all listed in `system:/elektra/modules/<plugin>/exports/*` and the `<plugin>` part of the name is the `name` of the plugin.

## Constraints

## Assumptions

## Considered Alternatives

1. Create a separate `kdbContract` that returns the contract of a plugin.

   ```c
   KeySet * ELEKTRA_PLUGIN_FUNCTION(contract) ();

   // OR

   void ELEKTRA_PLUGIN_FUNCTION(contract) (KeySet * ks);
   ```

   This solves the problem of the awkward `get` function, but it does not address the issue of the duplicate information in `ELEKTRA_PLUGIN_EXPORT`.

2. Change the `ELEKTRA_PLUGIN_EXPORT` to return the contract of the plugin instead of creating half a `struct _Plugin`.

   ```c
   KeySet * ELEKTRA_PLUGIN_EXPORT ();

   // OR

   void ELEKTRA_PLUGIN_EXPORT (KeySet * ks);
   ```

   This also removes the duplicate information inside the plugin.

3. (combined with 2) Change what the `KeySet * modules` stores.

   Currently, `modules` stores data dependent on how modules are loaded.
   When dynamic linking is used, the keys store the handle from `dlopen` and a pointer to the `ELEKTRA_PLUGIN_EXPORT` function.
   With static linking, the keys store a custom struct with function pointers.

   Instead, we would now store the contracts directly in `KeySet * modules`.

   The implementations of `elektraModulesLoad` would simply find the correct function and call it.

   ```c
   typedef void (*elektraPluginExportFn) (KeySet * ks);

   void elektraModulesLoad (KeySet * modules, const char * name, Key * errorKey)
   {
        Key * moduleKey = keyNew ("system:/elektra/modules", KEY_END);
   keyAddBaseName (moduleKey, name);
   if (ksLookup (modules, moduleKey, 0) != NULL)
   {
                // already loaded
   	return;
   }

        elektraPluginExportFn exportFn;
        // [...] find exportFn for plugin

        exportFn (modules); // inserts the contract with all the exported functions

        if (ksLookup (modules, moduleKey, 0) == NULL)
   {
                ksAppendKey (modules, moduleKey);
   }
        else
        {
                keyDel (moduleKey);
        }
   }
   ```

   > **Note** to avoid allocated and deleting a temporary `KeySet *` the `void ELEKTRA_PLUGIN_EXPORT (KeySet * ks)` API works best here.

## Decision

## Rationale

## Implications

## Related Decisions

- [Commit Function](commit_function.md)
- [Plugin Struct](../0a_postponed/plugin_struct.md)

## Notes
