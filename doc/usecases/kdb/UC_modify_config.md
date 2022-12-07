# Use Case: Modifying application configuration

## Summary

- **Scope:** `libelektra-kdb`
- **Level:** Developer Goal
- **Actors:** Application, Elektra
- **Brief:** Application modifies configuration

## Scenarios

- **Precondition:** -
- **Main success scenario:**
  - [Application loads configuration](UC_get_config.md)
  - Application modifies configuration in memory
  - [Application stores configuration](UC_set_config.md)
- **Alternative scenario:** -
- **Error scenario:** -
  > **Note**: The error scenarios from the references use cases still apply.
  > However, the in-memory modification part itself has no error scenario.
- **Postcondition:**
  - The modified configuration MUST be stored into the KDB exactly.
    Loading the configuration later (assuming no outside modifications) MUST return the modified configuration.
  - In case of error, the KDB MUST NOT be modified.
    Loading the configuration later (assuming no outside modifications) MUST return the unmodified configuration.
- **Non-functional Constraints:**
  - Modifying configuration MUST only touch the backends that have changed data.
