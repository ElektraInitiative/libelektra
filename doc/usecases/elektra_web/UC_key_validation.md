# Use Case: Key validation

## Summary

- **Title:** Key validation
- **Scope:** Configuration
- **Level:** User Goal
- **Actors:** User (usually a sysadmin)
- **Brief:** User gets feedback about a key's contents.

## Scenarios

- **Extends:** Modifying keys, Adding keys, Drag & Drop keys.
- **Main success scenario:** If the entered data passes the validation, it is
  sent to the backend and saved to the key database.
- **Alternative scenario:** User enters data that fails the field validation,
  the input field is marked as invalid (e.g. with a red color and some
  information text) and the data is NOT sent to the backend.
- **Alternative scenario:** User enters data that violates a validation rule,
  an error message is shown and the data is NOT saved to the key database.
- **Error scenario:** Technical problems while persisting to the key database.
  The user is informed about the problem.
- **Postcondition:** The updated key is persisted to the database.
- **Non-functional Constraints:** -
