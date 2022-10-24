# Commit Function

## Problem

When `kdbSet()` is called, plugins implementing the commit role, need to internally track their state to distinguish between carrying out that role and carrying out potential other roles (commit and setresolver for the resolver plugin, for example).
This limits the possibilities of plugin reuse and the ways plugins can be combined.

More generally, this problem applies to all plugins with one function that is called multiple times for different reasons.

Between `libelektra-kdb` and a backend plugins there is a [contract](../dev/backend-plugins.md) that uses "phases" for this.

With the introduction of the new backend, a new `kdbCommit` function was introduced to `struct _Plugin` (matching `kdbError`).
We also added a `kdbInit`.
Therefore, some phases of `kdbGet`/`kdbSet` now use separate functions, but not all of them do.
This partial use of separate functions can be weird and unintuitive.

## Constraints

- Plugins should not have internal state that tracks how often a function has been called to determine what the function should do next.

## Assumptions

## Considered Alternatives

1. Keep the current "partial separation" approach.

   So far, there has been no technical reason why some phases use separate functions and others do not.
   At least there has been no explanation, why separate `kdbCommit`/`kdbError` would be better than having some other way of knowing the phase.

2. Fully separate the functions with one function for each of the phases `libelektra-kdb` knows.

   This would introduce a lot of symbols, which would bloat `struct _Plugin`.
   The bloat could be avoided by choosing the approach in the ["Plugin Contract Function"](plugin_contract_function.md) decision that removes the function pointers from `struct _Plugin`.

3. Only use `kdbOpen`, `kdbGet`, `kdbSet` and `kdbClose` functions matching the ones in `libelektra-kdb`.
   Communicate the current phase via the `elektraPluginGetPhase` function.

   Matching the functions from `libelektra-kdb` makes it clear, which plugin function gets used for which `kdb*` call.
   The API does not make it obvious that phases exist.
   Plugins that handle multiple phases must call `elektraPluginGetPhase` to know what phase is being executed.
   Also using a single `elektraPluginGetPhase` function means all phases must share a single type, even if not all phases are shared between `kdbGet` and `kdbSet`.

4. Like 3, but instead of using a `elektraPluginGetPhase` function, directly pass a `ElektraGetPhase phase`/`ElektraSetPhase phase` parameter.

   This API makes it very clear that phases exist and that the plugin might need to handle them.
   It also allows using separate types for the phases of `kdbGet` and `kdbSet`.
   However, a big downside of this approach is that it adds an extra parameter to the API that will be unused by many plugins.

   This API also allows plugins to call other plugins to execute a different phase.
   This could be a good thing or a bad thing.

## Decision

## Rationale

## Implications

## Related Decisions

- [Plugin Contract Function](plugin_contract_function.md)
- [Plugin Struct](plugin_struct.md)

## Notes
