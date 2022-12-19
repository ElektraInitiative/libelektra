# Use Case: Cut `Key` hierarchy from `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller cuts (=looks up and removes) a `Key` hierarchy from an existing `KeySet`

## Scenarios

- **Precondition:**
  - [`KeySet` has been created](UC_keyset_create.md)
- **Main success scenario:**
  - Caller asks Core to cut a `Key` hierarchy descendant from a given `Key` R from a `KeySet` KS.
  - Core searches for the first `Key` in KS that is part of the desired hierarchy, i.e., the first `Key` in KS that is a descendant of R according to [`Key` hierarchy comparison](UC_keyname_hierarchy.md).
  - If not found, Core returns an empty KS
  - If found, Core searches for the last `Key` that is not part of the desired hierarchy, i.e., Core tries all `Key`s in KS in [keyname order](UC_keyname_ordering.md) until it finds a `Key` that is not a descendant of R.
  - Core removes all `Key`s of the desired hierarchy from KS and puts them into a new `KeySet` KS1
  - Core returns KS1.
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:** -
