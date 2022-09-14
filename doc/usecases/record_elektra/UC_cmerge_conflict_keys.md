# Use Case: Get information which keys are conflicting in 3-way-merge

## Summary

- **Title:** Get information which keys are conflicting in 3-way-merge
- **Scope:** Library/Development
- **Level:** Subfunction
- **Actors:** Developers
- **Brief:** It should be possible to get information about which keys are causing conflicts in a 3-way-merge.

## Scenarios

- **Precondition:** -
- **Main success scenario:** If there is a conflict, the developer should be able to call an API that returns reasonable information wich keys are causing a conflict.
- **Alternative scenario:** If no conflict occured, the API should indicate so.
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:**
  - Getting information about the conflict should not alter the result
