# Use Case: Loading application configuration

## Summary

- **Title:** Loading application configuration
- **Scope:** `libelektra-kdb`
- **Level:** Developer Goal
- **Actors:** Application, Elektra
- **Brief:** Application loads configuration from KDB

## Scenarios

- **Precondition:** -
- **Main success scenario:**
  - Application requests current configuration from KDB
  - Elektra loads from backends
  - Elektra returns configuration
- **Alternative scenario:**
  - Application requests current configuration from KDB
  - Elektra detects configuration hasn't changed and is already present in memory (of current instance)
  - Elektra returns previously loaded configuration
- **Error scenario:**
  - Application requests current configuration from KDB
  - Elektra fails to load requested configuration
  - Elektra returns error with problem description
- **Postcondition:**
  - The returned configuration MUST exactly match the requested part of the KDB
- **Non-functional Constraints:**
  - Applications SHOULD be able to err on the side of "better load config more often than needed" rather than "only load when necessary".
    - Loading configuration MUST be reasonably fast, especially when nothing has changed (caching!).
    - Reloading configuration MUST NOT destroy previously loaded configuration.
