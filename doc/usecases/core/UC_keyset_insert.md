# Use Case: Insert `Key` into `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller inserts new `Key` into existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md)
- **Main success scenario:**
  - Caller requests to insert a new `Key` with non-cascading name into `KeySet`
  - Namespace of `Key` is allowed by type of `KeySet`.
  - If necessary Core resizes `KeySet`
  - If necessary Core removes existing `Key` with same name
  - Core adds `Key` to `KeySet`
- **Alternative scenario:**
  - Caller requests to insert all `Key`s contained in one `KeySet` A into another `KeySet` B
  - Type of A matches type of B.
  - If necessary Core resizes B
  - If necessary Core removes all existing `Key`s contained in A and B from B
  - Core copies all `Key`s from A to B
- **Error scenario:**
  - Caller requests to insert a new `Key` with non-cascading name into `KeySet`
  - Namespace of `Key` is prohibited by type of `KeySet`.
  - Core aborts and returns error; `KeySet` is unmodified.
- **Postcondition:**
  - The new `Key`(s) provided by Caller MUST be part of `KeySet` exactly once.
    It MUST be possible to find exactly that/those `Key`(s) via a lookup.
- **Non-functional Constraints:** -
