# Use Case: Index access to `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller accesses contents of `KeySet` by index

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md).
  - [`Key` has been inserted into `KeySet`](UC_keyset_insert.md).
- **Main success scenario:**
  - Caller requests `Key` at valid index (`0 <= i < size`) from `KeySet`
  - Core returns `Key *` for `Key` at index
- **Alternative scenario:** -
- **Error scenario:**
  - Caller requests `Key` at invalid index (`i < 0 || i >= size`) from `KeySet`
  - Core returns `NULL`
- **Postcondition:**
  - The returned `Key *` MUST be valid until the `Key` is removed from the `KeySet`.
- **Non-functional Constraints:** -
