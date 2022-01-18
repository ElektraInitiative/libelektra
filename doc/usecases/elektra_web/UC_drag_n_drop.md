# Use Case: Drag & Drop keys

## Summary

- **Title:** Drag & Drop keys
- **Scope:** Configuration
- **Level:** User Goal
- **Actors:** User (usually a sysadmin)
- **Brief:** User drags a key (with a subtree) to a different subtree.

## Scenarios

- **Precondition:** View configuration of an instance.
- **Main success scenario:** User moves a key (and its subtree) to another
  subtree by dragging it to the desired position.
- **Alternative scenario:** User enters data that violates a validation rule,
  an error message is shown.
- **Error scenario:** Technical problems while persisting to the key database.
  The user is informed about the problem.
- **Postcondition:** The updated key structure is persisted to the database.
- **Non-functional Constraints:** -
