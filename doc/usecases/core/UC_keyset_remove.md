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
  - Caller requests to remove `Key` by index from `KeySet`
  - Index is in-range for `KeySet`
  - Core removes `Key` at index and resizes `KeySet`.
- **Alternative scenario:**
  - Caller requests to remove a range of `Key`s by index range from `KeySet`
  - Whole index range is entirely in-range for `KeySet`
  - Core removes all `Key`s within index range and resizes `KeySet`.
- **Error scenario:**
  - Caller requests to remove `Key` by index I (or range of `Key`s by index range R) from `KeySet`.
  - I (or R) is (partially) out-of-range for `KeySet`.
  - Core returns error value, `KeySet` is unmodified.
- **Postcondition:**
  - The removed `Key`(s) MUST NOT be part of `KeySet` anymore.
    A lookup for that/those `Key`s MUST NOT return any results.
- **Non-functional Constraints:** -
