# Use Case: Look up `Key` hierarchy from `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller looks up a `Key` hierarchy from an existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md)
- **Main success scenario:**
  - Caller asks Core to look up a `Key` hierarchy, which descends from a given `Key` R, from a `KeySet` KS.
  - Core searches for the first `Key` in KS that is part of the desired hierarchy, i.e., the first `Key` F in KS that is a descendant of R according to [`Key` hierarchy comparison](UC_keyname_hierarchy.md).
  - If not found, Core returns an out-of-range index range.
  - If found, Core searches for the first `Key` L that is not part of the desired hierarchy.
    This is equivalent to Core trying all `Key`s in KS after F in [keyname order](UC_keyname_ordering.md) until it finds a `Key` that is not a descendant of R.
  - Core returns an index range `[f, l)` (start inclusive, end exclusive), where `f` is the index of `Key` F and `l` the index of L.
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:** -
