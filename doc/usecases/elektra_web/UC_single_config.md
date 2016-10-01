# Use Case: Remote configuration of a single instance

## Summary

Title: Remote configuration of a single instance
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User configures a single instance via the client.

## Scenarios

Precondition: Add instance
Main success scenario: User successfully changes configuration of the instance.
Alternative scenario: Instance not online. The user is informed about the issue.
Error scenario: Technical problems while configuring the instance. The user is informed about the issue.
Postcondition: The updated configuration is persisted to the instance.
Non-functional Constraints:
	- Essential functionality
