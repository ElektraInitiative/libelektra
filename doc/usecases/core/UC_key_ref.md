# Use Case: `Key` reference counting

## Summary

- **Scope:** `libelektra-core`
- **Level:** Developer Goal
- **Actors:** Core, Caller
- **Brief:** Access to and manipulation of `Key`'s reference counter
- **Status:** Implemented

## Scenarios

- **Precondition:**
  - [`Key` has been created](UC_key_create.md)
- **Main success scenario:**
  - Caller requests value of reference counter of a `Key`
  - Core returns current value of reference counter of a `Key`
- **Alternative scenario:**
  - Caller requests to increment value of reference counter of a `Key`
  - Core returns current value of reference counter of a `Key` after incrementing
- **Alternative scenario:**
  - Caller requests to decrement value of reference counter of a `Key`
  - Core returns current value of reference counter of a `Key` after decrementing
- **Error scenario:**
  - Caller requests to increment value of reference counter of a `Key` whose reference counter is INT_MAX
  - Reference counter will not get incremented and stays at INT_MAX
- **Error scenario:**
  - Caller requests to decrement value of reference counter of a `Key` whose reference counter is 0
  - Reference counter will not get decremented and stays at 0
- **Postcondition:**
  - Reference Counter has a value from 0 to INT_MAX
- **Non-functional Constraints:**
