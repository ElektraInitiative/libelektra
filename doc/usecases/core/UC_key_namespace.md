# Use Case: `Key` Namespace

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Access to and manipulation of `Key`'s namespace

## Scenarios

- **Precondition:**
  - [`Key` has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller requests current namespace of `Key`
  - Core returns current namespace of `Key`
- **Alternative scenario:**
  - Caller requests to change namespace of `Key` to new namespace
  - Core stores new namespace in `Key`
- **Error scenario:**
  - `Key` has name marked as read-only (e.g., because `Key` is part of ordered collection like `KeySet`)
  - Caller requests to change namespace of `Key` to new namespace
  - Core returns error and namespace of `Key` remains unchanged
- **Postcondition:** -
- **Non-functional Constraints:** -
