# Use Case: Delete `Key` from `KeySet`

## Summary

- **Title:** Delete `Key` from `KeySet`
- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller removes `Key` from existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md)
  - [`Key` has been inserted into `KeySet`](UC_keyset_insert.md)
- **Main success scenario:**
  - Caller [looks up `Key` in `KeySet`](UC_keyset_lookup_basic.md) to get index
  - Caller requests to remove `Key` by index from `KeySet`
  - Core removes given index from `KeySet`
- **Alternative scenario:**
  - Caller [looks up hierarchy in `KeySet`](UC_keyset_lookup_prefix.md) to get index range
  - Caller requests to remove `Key`s by index range from `KeySet`
  - Core removes given index range from `KeySet`
- **Error scenario:** -
- **Postcondition:**
  - The deleted `Key`(s) MUST NOT be part of `KeySet` anymore.
    A lookup for that/those `Key`s MUST NOT return any results.
- **Non-functional Constraints:** -
