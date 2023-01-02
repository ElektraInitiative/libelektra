# Use Case: cli get configuration

## Summary

- **Scope:** CLI use
- **Level:** Developer Goal, UI
- **Actors:** CLI user
- **Brief:** User reads data for a given keyname

## Scenarios

- **Precondition:** -
- **Main success scenario:**
  - Backend is asked for the data for the given keyname
  - the data is outputted
- **Alternative scenario:** -
- **Error scenario:**
  - The keyname does not exist
  - The keyname is not valid
  - The keyname contains a bookmark that does not exist
- **Postcondition:**
  - Data for keyname or error-message is outputted
