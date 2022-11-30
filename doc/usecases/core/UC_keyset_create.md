# Use Case: Create `KeySet`

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Caller creates new `KeySet` instance

## Scenarios

- **Precondition:** -
- **Main success scenario:**
  - Caller requests to create a new `KeySet`
  - Core instantiates an emtpy `KeySet` and returns it
- **Alternative scenario:**
  - Caller requests to create a new `KeySet`, with known content
  - Core instantiates an appropriately sized `KeySet`, fills it and returns it
- **Error scenario:** -
- **Postcondition:**
  - `KeySet` exists and is usable by Caller
  - `KeySet` optionally has initial contents defined by Caller
- **Non-functional Constraints:**
  - `KeySet` MUST be resizable after creation
  - `KeySet` SHOULD be efficient for small and larger sizes
