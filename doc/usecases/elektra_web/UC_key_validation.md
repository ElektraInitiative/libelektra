# Use Case: Key validation

## Summary

Title: Key validation
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User gets feedback about a key's contents.

## Scenarios

Precondition: Modifying keys
Main success scenario: User edits a key's contents in a field that is
  specifically formatted according to its metadata (e.g. number/date fields vs
  string fields with regex validation). If the entered data passes the
  validation, it is sent to the backend and saved to the key database.
Alternative scenario: User enters data that fails the validation, the input
  field is marked as invalid (e.g. with a red color and some information text)
  and the data is NOT sent to the backend.
Error scenario: Technical problems while persisting to the key database. The
  user is informed about the issue.
Postcondition: The updated key is persisted to the database.
Non-functional Constraints:
	- Essential functionality
