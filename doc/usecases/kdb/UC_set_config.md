# Use Case: Set configuration

## Summary

- **Scope:** `libelektra-kdb`
- **Level:** Developer Goal
- **Actors:** Application, Elektra
- **Brief:** Application stores configuration into KDB

## Scenarios

- **Precondition:** -
- **Main success scenario:**
  - Application asks to store new configuration into KDB
  - Elektra stores configuration in backends
- **Alternative scenario:** -
- **Error scenario:**
  - Application asks to store new configuration into KDB
  - Elektra fails to store configuration
  - Elektra returns error with problem description
- **Postcondition:**
  - The provided configuration MUST be stored into the KDB exactly.
    Loading the configuration later (assuming no outside modifications) MUST return the provided configuration.
  - In case of error, the KDB MUST NOT be modified.
    Loading the configuration later (assuming no outside modifications) MUST return the same configuration as before the attempt to store new configuration.
- **Non-functional Constraints:**
  - Storing configuration MUST only touch the backends that have new data.
