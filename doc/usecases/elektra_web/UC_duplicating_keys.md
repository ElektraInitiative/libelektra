# Use Case: Duplicating keys

## Summary

Title: Duplicating keys
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User duplicates an existing key (and its subtree).

## Scenarios

Precondition: View configuration of an instance
Main success scenario: User duplicates an existing key (and its subtree).
Alternative scenario: User enters data that violates a validation rule, an error
  message is shown.
Error scenario: Technical problems while persisting to the key database. The
  user is informed about the issue.
Postcondition: The new key is persisted to the database.
Non-functional Constraints: -
