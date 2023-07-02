# Use Case: `Key` copy

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Copying one or more fields from one `Key` to another
- **Status:** Implemented

## Scenarios

- **Precondition:**
  - [`Key` K1 has been created](UC_key_create.md)
  - [`Key` K2 has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller requests to copy one or multiple fields from `Key` k1 to `Key` k2
  - Fields that can be copied are name, value and metadata
  - Fields, that caller wanted to copy, of `Key` k2 now have the value of the respective fields of `Key` k1
- **Alternative scenario:**
- **Error scenario:**
  - `Key` k2 has field that should be copied marked as read-only (e.g., because `Key` is part of ordered collection like `KeySet`)
  - Caller requests to copy read-only field in `Key` k2 from `Key` k1
  - Copying to k2 fails and k2 retains the old field values
- **Postcondition:**
  - [Comparing](UC_keyname_hierarchy.md) the respective fields from k1 and k2 returns that those fields are equal
- **Non-functional Constraints:**
