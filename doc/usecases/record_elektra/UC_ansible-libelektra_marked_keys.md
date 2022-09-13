# Use Case: Fail application of ansible-libelektra if certain keys differ in value

## Summary

- **Title:** Fail application of ansible-libelektra if certain keys differ in value
- **Scope:** ansible-libelektra
- **Level:** User goal
- **Actors:** User (usually sysadmin)
- **Brief:** We should be able to mark certain keys in the tasks for a specific value. If the keys do have a different value on the target machine, ansible-libelektra should fail.

## Scenarios

- **Precondition:** The user marks a certain key in the task
- **Main success scenario:** The key on the target machine has a different value or is not present -> fail the execution
- **Alternative scenario:** The key on the target machine has the same value -> continue the execution
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:** -
