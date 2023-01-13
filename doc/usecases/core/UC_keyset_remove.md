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
  - Caller requests to remove a range of `Key`s defined by a valid index range (`[f, l)` with `0 <= f < size && f <= l <= size`) from `KeySet`
  - Core removes all `Key`s within index range and resizes `KeySet`.
- **Alternative scenario:**
  > **Note**: Deleting a single `Key` given an index `i` is achieved, by deleting the index range `[i, i+1)`.
- **Error scenario:**
  - Caller requests `Key` to remove `Key`s in an invalid index range (`f < 0 || f >= size || l < f || l > size`) from `KeySet`
  - Core returns error value, `KeySet` is unmodified.
- **Postcondition:**
  - The removed `Key`s MUST NOT be part of `KeySet` anymore.
    A lookup for that/those `Key`s MUST NOT return any results.
- **Non-functional Constraints:**
  - Worst case runtime should be `O(n * m)` where `n = size`, the size of the `KeySet`, and `M = l - f`, the number of keys being deleted.
    We allow `O(n)` for deleting a single `Key`, because that is the expected runtime of resizing the `KeySet`.
