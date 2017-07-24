# Use Case: Adding keys

## Summary

- **Title:** Adding keys
- **Scope:** Configuration
- **Level:** User Goal
- **Actors:** User (usually a sysadmin)
- **Brief:** User adds a key (to a subtree).

## Scenarios

- **Precondition:** View configuration of an instance.
- **Main success scenario:** User creates a new key (in a subtree).
- **Alternative scenario:** User enters data that violates a validation rule,
  an error message is shown.
- **Error scenario:** Technical problems while persisting to the key database.
  The user is informed about the problem.
- **Postcondition:** The new key is persisted to the database.
- **Non-functional Constraints:** -
