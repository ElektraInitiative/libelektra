# Use Case: Lookup hierarchy in `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller looks up `Key` hierarchy in existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md)
- **Main success scenario:**
  - Caller asks Core to look up `Key` hierarchy, i.e., all descendants of a `Key`, by non-cascading name prefix in `KeySet`
  - Core searches for first `Key` in `KeySet` that is part of the desired hierarchy
  - If not found, Core returns empty `KeySet`
  - If found, Core searches for last `Key` that is not part of the desired hierarchy.
  - Core returns `KeySet` with all `Key`s of the desired hierarchy.
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:** -
