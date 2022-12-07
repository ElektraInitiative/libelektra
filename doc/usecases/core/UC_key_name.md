# Use Case: `Key` Name

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Access to and manipulation of `Key`'s name

## Scenarios

- **Precondition:**
  - [`Key` has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller requests current name of `Key`
  - Core returns current name of `Key`
- **Alternative scenario:**
  - Caller requests to change name of `Key` to new name
  - Core stores new name in `Key`
- **Error scenario:**
  - `Key` has name marked as read-only (e.g., because `Key` is part of ordered collection like `KeySet`)
  - Caller requests to change name of `Key` to new name
  - Core returns error and name of `Key` remains unchanged
- **Postcondition:** -
- **Non-functional Constraints:** -
