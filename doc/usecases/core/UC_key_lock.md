# Use Case: `Key` lock

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Access to and manipulation of `Key`'s lock flags
- **Status:** Implemented

## Scenarios

- **Precondition:**
  - [`Key` has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller requests lock status of one or multiple flags of a `Key`
  - Possible lock flags are: name, value, metadata
  - Core returns for which lock flags the lock bit is set in the `Key`
- **Alternative scenario:**
  - Caller requests to set one or multiple lock flags for `Key`
  - Possible lock flags are: Name, Value, Meta
  - Core sets bits for lock flags in the `Key`
- **Error scenario:**
- **Postcondition:**
  - Locked fields cannot be changed via other functions from the Elektra Core API
- **Non-functional Constraints:**
