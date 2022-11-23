# Use Case: Lookup hierarchy in `KeySet`

## Summary

- **Title:** Lookup hierarchy in `KeySet`
- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller looks up `Key` hierarchy in existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md)
- **Main success scenario:**
  - Caller asks Core to look up `Key` hierarchy by non-cascading name prefix in `KeySet`
  - Core searches for index S of first `Key` in `KeySet` that is part of the desired hierarchy
  - If not found, Core returns empty range
  - If found, Core searches for (theoretical) index E of next following `Key` that is not part of the desired hierarchy.
  - Core returns half-open index range [S, E).
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:**
  - The returned index values MUST be valid and correct until new `Key`(s) are inserted into or deleted from the `KeySet`.
- **Non-functional Constraints:** -
