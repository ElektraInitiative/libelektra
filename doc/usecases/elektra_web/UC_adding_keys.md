# Use Case: Adding keys

## Summary

Title: Adding keys
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User adds a key (to a subtree).

## Scenarios

Precondition: Add instance/cluster
Main success scenario: User creates a new key (in a subtree) and can set its metadata.
Alternative scenario: User duplicates an existing key (and its subtree).
Error scenario: Technical problems while persisting to the key database. The user is informed about the issue.
Postcondition: The new key is persisted to the database.
Non-functional Constraints:
	- Essential functionality
