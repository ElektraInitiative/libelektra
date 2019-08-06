# Use Case: Create Configuration Snippet

## Summary

- **Title:** Create configuration snippet
- **Scope:** Entry Management
- **Level:** User Goal
- **Actors:** Authenticated user
- **Brief:** User can create a new configuration snippet (database entry) based on a set of specified inputs, one of them being the snippet itself.

## Scenarios

- **Precondition:** Authenticated, sufficient permissions
- **Main success scenario:** All inputs validate and the configuration snippet could be parsed and stored in the Elektra database successfully. The user is then redirected to the detailed view of the created snippet.
- **Alternative scenario:** Wrong inputs lead to an error message and ask the user to correct them, then they can try again. It is also possible that the supplied configuration snippet is of an unsupported format. In either case the user retrieves a response containing error information.
- **Error scenario:** Technical problems prevent the storage process from completing. The user is informed about the issue.
- **Postcondition:** Configuration snippet is stored in the database successfully and can be found through search and direct link.
- **Non-functional Constraints:**
  - Essential functionality of the service
