# Use Case: cli set configuration

## Summary

- **Scope:** CLI use
- **Level:** Developer Goal
- **Actors:** Command Line User
- **Brief:** User sets a new config in tge user-namespace

## Scenarios

- **Precondition:** -
- **Main success scenario:**
  - CLI tells backend to save data given by the user to the key the user specified
  - the backend writes the data.
- **Alternative scenario:** -
- **Error scenario:**
  - The keyname is not properly formated
  - or a non existant bookmark is used
- **Postcondition:** The data is now in the key database
