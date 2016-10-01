# Use Case: Add instance

## Summary

Title: Add instance
Scope: Setup
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User sets up elektrad on the instance and connects it to clusterd according to the corresponding README.

## Scenarios

Precondition: Setup
Main success scenario: User successfully connects elektrad instance to clusterd.
Alternative scenario: Authentication issues when connecting the services, e.g. wrong api key.
Error scenario: Technical problems while setting up elektrad. The user is informed about the issue.
Postcondition: User can now access the instance via the client.
Non-functional Constraints:
	- Security mechanism
	- Is precondition to use other functions (single config, multiple config)
