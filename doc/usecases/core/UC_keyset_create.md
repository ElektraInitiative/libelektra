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
  - Core instantiates an empty `KeySet` and returns it
- **Alternative scenario:** -
- **Error scenario:** -
- **Postcondition:**
  - `KeySet` exists and is usable by Caller
- **Non-functional Constraints:**
  - `KeySet` MUST be resizable after creation
  - `KeySet` SHOULD be efficient for small (~10) to larger (~100.000) sizes
