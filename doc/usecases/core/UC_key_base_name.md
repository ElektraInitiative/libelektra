# Use Case: `Key` Basename

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Access to and manipulation of `Key`'s basename
- **Status:** Implemented

## Scenarios

- **Precondition:**
  - [`Key` has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller requests current basename of `Key`
  - Core returns current basename of `Key`
- **Alternative scenario:**
  - Caller requests to change basename of `Key` to new basename
  - Core stores new basename in `Key`
- **Alternative scenario:**
  - Caller requests to append new name part to `Key`
  - Core stores appended name in `Key`
- **Error scenario:**
  - `Key` has name marked as read-only (e.g., because `Key` is part of ordered collection like `KeySet`)
  - Caller requests to change basename of `Key` to new basename
  - Core returns error and basename of `Key` remains unchanged
- **Error scenario:**
  - `Key` has name marked as read-only (e.g., because `Key` is part of ordered collection like `KeySet`)
  - Caller requests to append to name of `Key`
  - Core returns error and basename of `Key` remains unchanged
- **Postcondition:** -
  - `Key` has a valid name
- **Non-functional Constraints:** -
