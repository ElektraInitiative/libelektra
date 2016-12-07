# Use Case: Finding keys

## Summary

Title: Finding keys
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User searches for a key by filtering the key database.

## Scenarios

Precondition: Adding keys
Main success scenario: User searches for a key by entering information (its path
  or metadata) in a field. The whole key database is filtered according to the
  contents of the search field.
Alternative scenario: No results found, the user is informed about this.
Error scenario: Technical problems while persisting to the key database. The
  user is informed about the issue.
Postcondition: The filtered keys can be modified.
Non-functional Constraints: -
