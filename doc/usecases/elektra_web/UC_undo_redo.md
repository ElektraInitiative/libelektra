# Use Case: Undo/Redo

## Summary

- **Title:** Undo/Redo
- **Scope:** Configuration
- **Level:** User Goal
- **Actors:** User (usually a sysadmin)
- **Brief:** User undos/redos changes to a configuration.

## Scenarios

- **Precondition:** Modifying keys, Adding keys, Drag & Drop keys.
- **Main success scenario:** User successfully undos/redos a configuration
  change.
- **Error scenario:** Technical problems while configuring the instance.
  The user is informed about the problem.
- **Postcondition:** The updated configuration is persisted to the instance.
- **Non-functional Constraints:**
  - Every editor operation (preconditions) should be undoable
