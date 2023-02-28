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
  - If a matching `Key` is found, Core returns its index.
  - Otherwise, Core returns a negative value, which indicates "not found".
    Additionally, the negative value can be converted (with a well-defined formula) into the index at which the searched `Key` would be inserted.
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:**
  - The returned index value MUST be valid and correct until new `Key`(s) are inserted into or removed from the `KeySet`.
- **Non-functional Constraints:** -
