# Use Case: Undo/Redo

## Summary

Title: Undo/Redo
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User undos/redos changes to a configuration.

## Scenarios

Precondition: Modifying keys, Adding keys, Duplicating keys, Drag & Drop keys
Main success scenario: User successfully undos/redos a configuration change.
Alternative scenario: Instance not online. The user is informed about the issue.
Error scenario: Technical problems while configuring the instance. The user is
  informed about the issue.
Postcondition: The updated configuration is persisted to the instance/s.
Non-functional Constraints:
	- Every editor operation (preconditions) should be undoable
