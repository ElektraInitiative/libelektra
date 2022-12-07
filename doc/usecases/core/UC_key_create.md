# Use Case: Create `Key`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller creates new `Key` instance

## Scenarios

- **Precondition:**
  - The name used to create a `Key` must be valid
- **Main success scenario:**
  - Caller requests to create a new `Key` with a given name
  - Core instantiates a `Key` with the given name, no value and no metadata and returns it
- **Alternative scenario:**
  - Caller requests to create a new `Key`, with a given name and a value and/or metadata
  - Core instantiates a `Key`, with the given name, value, and metadata and returns it
- **Error scenario:** -
- **Postcondition:**
  - `Key` exists and is usable by Caller
  - `Key` has name defined by Caller
  - `Key` has value defined by Caller, if given, and no value otherwise
  - `Key` has metadata defined by Caller, if given and no metadata otherwise
- **Non-functional Constraints:** -
