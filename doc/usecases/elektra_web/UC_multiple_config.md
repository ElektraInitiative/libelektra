# Use Case: Remote configuration of a multiple instances

## Summary

Title: Remote configuration of a multiple instances
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User configures multiple (standalone) instances via the client.

## Scenarios

Precondition: Add instance (multiple times)
Main success scenario: User successfully changes configuration of multiple instances.
Alternative scenario: Instance not online. The user is informed about the issue.
Error scenario: Technical problems while configuring one of the instances. The user is informed about the issue.
Postcondition: The updated configurations are persisted to the instances.
Non-functional Constraints:
	- Essential functionality
