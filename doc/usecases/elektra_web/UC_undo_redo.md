# Use Case: Undo/Redo

## Summary

Title: Undo/Redo
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User undos/redos changes to a configuration.

## Scenarios

Precondition: Single/Cluster config
Main success scenario: User successfully undos/redos a configuration change.
Alternative scenario: Instance not online. The user is informed about the issue. If a cluster, changes will be written when the affected instance comes back online.
Error scenario: Technical problems while configuring the instance. The user is informed about the issue. If a cluster, changes will be written when the affected instance comes back online.
Postcondition: The updated configuration is persisted to the instance/s.
Non-functional Constraints:
	- Essential functionality
