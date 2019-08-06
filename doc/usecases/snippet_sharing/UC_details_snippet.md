# Use Case: View Details for Specific Configuration Snippet

## Summary

- **Title:** View details for specific configuration snippet
- **Scope:** Entry Details
- **Level:** User Goal
- **Actors:** Anonymous user (+ all others)
- **Brief:** Any user can view all details about an entry (configuration snippet).

## Scenarios

- **Preconditions:** Snippet has been found through search or URI is known
- **Main success scenario:** User sees detailed information of the requested entry, including tags and other meta information like a description, as well as the configuration snippet converted into all supported formats.
- **Alternative scenario:** The requested entry (URI) cannot be found, therefore the server responds with an error which is displayed to the user. If necessary, the user is redirected to the snippet overview.
- **Error scenario:** Technical problems prevent server from responding with detailed information. The user is informed about the issue.
- **Postcondition:** The database remains unchanged.
- **Non-functional Constraints:**
  - Essential functionality of the program
