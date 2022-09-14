# Use Case: Assert that certain keys contain specific values

## Summary

- **Title:** Assert that certain keys contain specific values
- **Scope:** ansible-libelektra
- **Level:** User goal
- **Actors:** System administrator
- **Brief:** There should be a functionality in ansible-libelektra to assert certain keys have certain values.
For example, some app B requires that a certain app A is already set up and running before configuring can be done.
It may also be used to check that a certain plugin (e.g. `check`) is correctly installed within Elektra.

## Scenarios

- **Precondition:** The user asserts a certain value for a key in the task
- **Main success scenario:** The key on the target machine meets the assertion -> continue the execution
- **Alternative scenario:** The key on the target machine does not meet the assertion -> fail the execution
- **Error scenario:** -
- **Postcondition:** -
- **Non-functional Constraints:** -
