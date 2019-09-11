# Use Case: Download Configuration Snippet in Specific Format

## Summary

- **Title:** Download configuration snippet in specific format
- **Scope:** Entry Details
- **Level:** User Goal
- **Actors:** Anonymous user (+ all others)
- **Brief:** Any user can download the configuration snippets in various formats (XML, JSON, ini, ...).

## Scenarios

- **Precondition:** Entry has been found through search or detailed view of an entry is opened
- **Main success scenario:** User successfully downloads the raw configuration snippet in the selected format. The snippet can then be stored directly as configuration file.
- **Alternative scenario:** The specified entry (URI) cannot be found. The user is informed about the error by an error message.
- **Error scenario:** Technical problems prevent the operation from completing. The user is informed about the error by an error message containing further information.
- **Postcondition:** The database remains unchanged.
- **Non-functional Constraints:**
  - Essential functionality of the program
