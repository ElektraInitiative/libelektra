# Use Case: Index access to `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller accesses contents of `KeySet` by index

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md).
  - [`Key`(s) have been inserted into `KeySet`](UC_keyset_insert.md).
- **Main success scenario:**
  - Caller requests `Key`s in valid index range (`[f, l)` with `0 <= f < size && f <= l <= size`) from `KeySet` KS
  - Core creates new `KeySet` KS1
  - Core adds all `Key`s in KS with index `i >= f && i < l` to KS1.
  - Core returns KS1
- **Alternative scenario:** -
- **Error scenario:**
  - Caller requests `Key` at invalid index range (`f < 0 || f >= size || l < f || l > size`) from `KeySet`
  - Core returns `NULL`
- **Postcondition:**
  - The initial `KeySet` KS MUST be unmodified.
- **Non-functional Constraints:** -
