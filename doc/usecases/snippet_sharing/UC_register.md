# Use Case: Register User Account

## Summary

- **Title:** Register user account
- **Scope:** Authentication
- **Level:** User Goal
- **Actors:** Anonymous user
- **Brief:** Any not logged-in user can register a new user account.

## Scenarios

- **Precondition:**
- **Main success scenario:** The user successfully created a new account. The user is informed about the success.
- **Alternative scenario:** The inputs could not be validated (do not match the required constraints). The user is informed about their mistake.
- **Error scenario:** Technical problems prevent the registration from succeeding. The user is informed about the issue.
- **Postcondition:** The user account is created successfully in the database and the provided credentials can now be used to authenticate against the service.
- **Non-functional Constraints:**
  - Security mechanism
  - Is precondition to use other functions
