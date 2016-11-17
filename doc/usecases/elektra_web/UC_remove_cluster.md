# Use Case: Remove cluster

## Summary

Title: Remove cluster
Scope: Setup
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User removes a cluster from elektra-web.

## Scenarios

Precondition: Add cluster
Main success scenario: User successfully removes a cluster.
Alternative scenario: -
Error scenario: Technical problems while removing the cluster. The user is informed about the issue.
Postcondition: Cluster won't show up in the client anymore. The instances contained in the cluster show up as single instances again. Configuration defined by the cluster can now be configured in single instances again.
Non-functional Constraints:
	- Essential functionality
