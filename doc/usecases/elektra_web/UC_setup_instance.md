# Use Case: Setup Instance

## Summary

- **Title:** Setup instance
- **Scope:** Setup
- **Level:** User Goal
- **Actors:** User (usually a sysadmin)
- **Brief:** User sets up a daemon on the instance and connects it to elektra-web.

## Scenarios

- **Main success scenario:** User successfully sets up elektrad on the instance
  and connects it to elektra-web.
- **Alternative scenario:** Authentication issues when connecting the services,
  e.g. wrong API key.
- **Alternative scenario:** The instance is already connected to elektra-web. A
  message informs the user about this.
- **Error scenario:** Technical problems - the user is informed about the
  problem.
- **Postcondition:** User can now access the instance via the client.
- **Non-functional Constraints:**
  - Security mechanism
