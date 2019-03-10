# Use Case: Modifying keys

## Summary

- **Title:** Modifying keys
- **Scope:** Configuration
- **Level:** User Goal
- **Actors:** User (usually a sysadmin)
- **Brief:** User modifies a key's contents.

## Scenarios

- **Precondition:** View configuration of an instance.
- **Main success scenario:** User edits a key's contents in a field that is
  specifically formatted according to its metadata (e.g. number/date fields vs
  string fields with regex validation). Arrays are specifically formatted as
  lists of fields under a key. A description of the key is also shown.
- **Alternative scenario:** User enters data that fails the field validation,
  the input field is marked as invalid (e.g. with a red color and some
  information text) and the data is _not_ sent to the backend.
- **Error scenario:** Technical problems while persisting to the key database.
  The user is informed about the problem.
- **Postcondition:** The updated key is persisted to the database.
- **Non-functional Constraints:** -
