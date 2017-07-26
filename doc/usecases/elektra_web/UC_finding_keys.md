# Use Case: Finding keys

## Summary

- **Title:** Finding keys
- **Scope:** Configuration
- **Level:** User Goal
- **Actors:** User (usually a sysadmin)
- **Brief:** User searches for a key by filtering the key database.

## Scenarios

- **Precondition:** View configuration of an instance.
- **Main success scenario:**
  - User searches for a key by entering information (its path or value)
    in a field.
  - The whole key database is filtered according to the contents
    of the search field.
  - At first, only keys that match the path are shown. Then, the application
    will continue searching for values that contain the search input in the
    background, and show matches once they are found.
  - When the search input field is cleared, the whole key database will be shown
    again.
- **Alternative scenario:** No results found, the user is informed about this.
- **Error scenario:** -
- **Postcondition:** The filtered keys can be modified.
- **Non-functional Constraints:** -
