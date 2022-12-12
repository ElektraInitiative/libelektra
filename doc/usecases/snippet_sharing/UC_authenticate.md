# Use Case: Authenticate

## Summary

- **Title:** Authenticate
- **Scope:** Authentication
- **Level:** User Goal
- **Actors:** Anonymous user
- **Brief:** Any not logged-in user can authenticate to the service.

## Scenarios

- **Precondition:** Website opened
- **Main success scenario:** User successfully authenticates against the server. The websites changes its state to authenticated, which enables more features (e.g. creation of snippets).
- **Alternative scenario:** The authentication was not successful (e.g. wrong credentials). The user is prompted to check their credentials and retry.
- **Error scenario:** Technical problems prevent the authentication from succeeding (e.g. connection issues). The user is informed about the issue.
- **Postcondition:** Website is in authenticated state, meaning that all subsequent requests will use the authentication token to identify to the server.
- **Non-functional Constraints:**
  - Security mechanism
  - Is precondition to use other functions
