# Use Case: Modifying keys

## Summary

Title: Modifying keys
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User modifies a key's contents.

## Scenarios

Precondition: Key validation
Main success scenario: User edits a key's contents in a field that is
  specifically formatted according to its metadata (e.g. number/date fields vs
	string fields with regex validation). Arrays are specifically formatted as
	lists of fields under a key. A description of the key is shown as a tooltip
	next to the field. Metadata of keys can also be edited.
Alternative scenario: User deletes an existing key (which deletes its whole
	subtree).
Error scenario: Technical problems while persisting to the key database. The
  user is informed about the issue.
Postcondition: The updated key is persisted to the database.
Non-functional Constraints:
	- Essential functionality
