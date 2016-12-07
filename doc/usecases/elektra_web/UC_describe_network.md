# Use Case: Describe network topology

## Summary

Title: Describe network topology
Scope: Setup
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User renames instances and creates clusters to describe the network topology.

## Scenarios

Precondition: Setup instance
Main success scenario: User successfully renames instances and creates clusters
  of multiple instances to describe the network topology, e.g. having a cluster
	of web servers with the same configuration.
Alternative scenario: User removes an instance or a cluster. If a cluster is
  removed, the instances in it remain and are shown as single instances.
Error scenario: Technical problems while renaming or creating clusters. The user
  is informed about the issue.
Postcondition: User can now configure single instances or all instances in a
  cluster at once.
Non-functional Constraints:
	- Essential functionality
	- Is precondition for single config and cluster config
