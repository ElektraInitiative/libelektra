# Use Case: `Key` Value

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Access to and manipulation `Key`'s value

## Scenarios

- **Precondition:**
  - [`Key` has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller requests current value of `Key`
  - Core returns current value of `Key`
- **Alternative scenario:**
  - Caller requests to change value of `Key` to new value
  - Core stores new value in `Key`
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:** -
