# Use Case: Convert Configuration Snippet

## Summary

- **Title:** Convert configuration snippet
- **Scope:** Configuration conversion
- **Level:** User Goal
- **Actors:** Anonymous user (+ all others)
- **Brief:** User can input a configuration snippet in any supported format (e.g. ini, xml) and convert it into another supported format (e.g. JSON).

## Scenarios

- **Precondition:** Service supports the in- and output configuration format
- **Main success scenario:** All inputs validate and the configuration snippet could be converted into the target format. The user is then shown the result of the conversion - the input snippet in another format.
- **Alternative scenario:** Wrong inputs lead to an error message and ask the user to correct them, then they can try again. It is also possible that the supplied configuration snippet is of an unsupported format. In either case the user retrieves a response containing error information.
- **Error scenario:** Technical problems prevent the conversion from completing. The user is informed about the issue.
- **Postcondition:** No changes have been made to the database.
- **Non-functional Constraints:**
  - Show-case functionality of Elektra
