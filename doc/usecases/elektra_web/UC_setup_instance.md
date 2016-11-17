# Use Case: Setup instance

## Summary

Title: Setup instance
Scope: Setup
Level: User Goal
Actors: User (usually a sysadmin)
Brief: User sets up a daemon on the instance.

## Scenarios

Precondition: Setup
Main success scenario: User successfully sets up elektrad on the instance and connects it to elektra-web.
Alternative scenario: Authentication issues when connecting the services, e.g. wrong api key.
Error scenario: Technical problems while setting up elektrad. The user is informed about the issue.
Postcondition: User can now access the instance via the client.
Non-functional Constraints:
	- Security mechanism
	- Is precondition to use configuration functions
