# Use Case: Remote configuration of a cluster

## Summary

Title: Remote configuration of a cluster
Scope: Configuration
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User configures a cluster (multiple instances at once).

## Scenarios

Precondition: Describe network topology
Main success scenario: User successfully changes configuration of the cluster
  in an interactive tree view (see other Configuration use cases).
Alternative scenario: One of the instances is not online. The user is informed
  about the issue. Changes will be written when the instance comes back online.
Error scenario: Technical problems while persisting configuration to one of the
  instances. The user is informed about the issue and configuration will be
	written when the instance is reachable again.
Postcondition: The updated configuration is persisted to all instances of the
  cluster. Configuration defined by the cluster can't be set in single instances
  anymore.
Non-functional Constraints:
	- Essential functionality
