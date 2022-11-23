# Use Case: Retrieve `Key` from `KeySet`

## Summary

- **Title:** Retrieve `Key` from `KeySet`
- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller retrieves contained `Key` from existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md).
  - [`Key` has been inserted into `KeySet`](UC_keyset_insert.md).
  - Caller has known valid index for `KeySet`.
    This can be from a [lookup](UC_keyset_lookup_basic.md), but also via simple iteration for example.
- **Main success scenario:**
  - Caller requests `Key` at valid index from `KeySet`
  - Core returns `Key *` for `Key` at index
- **Alternative scenario:** -
- **Error scenario:**
  - Caller requests `Key` at invalid index from `KeySet`
  - Core returns `NULL`
- **Postcondition:**
  - The returned `Key *` MUST be valid until the `Key` is removed from the `KeySet`.
- **Non-functional Constraints:** -
