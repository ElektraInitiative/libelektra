# Use Case: Recording changes to the key database

## Summary

- **Title:** Recording changes to the key database
- **Scope:** Configuration/Logging
- **Level:** User goal
- **Actors:** User (usually a sysadmin)
- **Brief:** Any changes that are made in the key database are recorded.

## Scenarios

- **Precondition:**
  - Recording is enabled
  - An active recording session exists
- **Main success scenario:**
  - User makes a change to the key database, e.g. add key, delete key, modify key, modify meta
  - The old & new values for every chagned key and metakey are recorded.
- **Alternative scenario:** -
- **Error scenario:** We log that the changes could not be recorded.

- **Postcondition:** All changed keys and their original values are recorded.

- **Non-functional Constraints:**
  - The recording mechanismn should not noticably slow down Elektra.
