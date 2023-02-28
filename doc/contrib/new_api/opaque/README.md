# New API opaque

### Note on structs

Here we do not use any public structs in the core API at all.
We also treat names and values differently.

For values we don't have a type.
Functions that deal with values deal with them as separate `void *` and `size_t`.

For names we simply expose the opaque `ElektraNameCow` struct used internally as `ElektraName`.
To that end, we introduce public APIs for creating and deleting a `ElektraName`.
Because, `ElektraNameCow` is refcounted this can be done safely.

Because everything is opaque, we have the most flexibility with this approach.
Note, however, that at this moment there is no known case were a breaking change could be avoided with these opaque structs, but not with the ["by-value" variant](../byvalue/README.md)

A clear disadvantage here is that a name cannot live on the stack.
That means any API that might use temporary names (e.g., `elektraSetLookup`, or `elektraSetFindHierarchy`) is more difficult to use without memory leaks.
Either the caller must ensure to call `elektraNameDel`, or the implementation must do so.
If the implementation calls `elektraNameDel`, callers may need extra `elektraNameIncRef`/`elektraNameDecRef` calls.
