# Use Case: `Key` clear

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Clearing contents of a `Key`
- **Status:** Implemented

## Scenarios

- **Precondition:**
  - [`Key` has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller requests the fields of a `Key` should get cleard
  - Name, value and metadata of the `Key` are empty after clearing
- **Alternative scenario:**
- **Error scenario:**
  - `Key` has name, value or metadata marked as read-only (e.g., because `Key` is part of ordered collection like `KeySet`)
  - Caller requests to clear `Key`
  - Core returns error and name, value and metadata of `Key` remain unchanged
- **Postcondition:**
- **Non-functional Constraints:**
