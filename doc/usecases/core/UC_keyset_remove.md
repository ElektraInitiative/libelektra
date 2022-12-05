# Use Case: Remove `Key` from `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller removes `Key` from existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md)
- **Main success scenario:**
  - Caller requests to remove `Key` by name from `KeySet`
  - Core searches for `Key` with same name in `KeySet`
  - If a matching `Key` is found, Core removes it from `KeySet` returns a `Key *` to it.
    The name of the `Key` will be read-only, otherwise it is modifiable.
  - Otherwise, Core returns `NULL`
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:**
  - The removed `Key`(s) MUST NOT be part of `KeySet` anymore.
    A lookup for that/those `Key`s MUST NOT return any results.
- **Non-functional Constraints:** -
