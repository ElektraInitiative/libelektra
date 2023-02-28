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
  - If a matching `Key` is found, Core returns a value which encodes both
    - "`Key` found"
    - the current index of the `Key`.
  - Otherwise, Core returns a value which encodes both
    - "`Key` not found"
    - the index at which the searched `Key` would be inserted right now.
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:**
  - The returned index value MUST be valid and correct until new `Key`(s) are inserted into or removed from the `KeySet`.
- **Non-functional Constraints:** -
