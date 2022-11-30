# Use Case: Direct lookup in `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller looks up `Key` with non-cascading name in existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md)
- **Main success scenario:**
  - Caller asks Core to look up `Key` by non-cascading name in `KeySet`
  - Core searches for `Key` with same name in `KeySet`
  - If found, Core returns index
  - Otherwise, Core returns a value indicating both "Not found" and the index, where a `Key` with the given name would be inserted.
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:**
  - The returned index value MUST be valid and correct until new `Key`(s) are inserted into or deleted from the `KeySet`.
- **Non-functional Constraints:** -
