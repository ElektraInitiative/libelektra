# Use Case: Ensuring valid configuration

## Summary

- **Title:** Ensuring valid configuration
- **Scope:** `libelektra-kdb`, `spec`
- **Level:** Developer Goal, User Goal
- **Actors:** Elektra, Application, User
- **Brief:** After loading configuration, Application can assume the configuration is valid

## Scenarios

- **Precondition:**
  - A specification defining what a valid configuration for Application looks like exists.
- **Main success scenario:**
  - [Application loads configuration](UC_load_config.md)
    During this process Elektra checks the configuration against the specification.
  - Loading configuration results in no errors or warnings.
  - Application knows configuration is valid.
- **Alternative scenario:**
  - User requests to change configuration value via [Application (e.g. `kdb`) stores configuration](UC_store_config.md).
    During this process Elektra checks the changed configuration against the specification.
  - Elektra blocks the modification, if it violates the specification.
- **Error scenario:**
  - [Application loads configuration](UC_load_config.md)
    During this process Elektra checks the configuration against the specification.
  - Loading configuration results in an error or warning.
  - If a configuration is returned at all, Application knows the configuration may not be fully valid and can react accordingly.
- **Postcondition:**
  - Configuration stored in the KDB and modified only via Elektra MUST be valid according to the specification.
  - Configuration loaded by an application _without_ error or warning MUST be valid according to the specification.
  - Configuration loaded by an application _with_ error or warning MUST be valid according to the specification, except where an error or warning indicates otherwise.
- **Non-functional Constraints:**
  - Because validation happens during [Application loads configuration](UC_load_config.md), it MUST also be reasonably performant.
    Results MAY be cached to achieve this performance.
  - Fixing an invalid configuration via Elektra MUST be possible.
    This SHOULD NOT require more changes than necessary, i.e., fixing invalid configuration MUST NOT be equivalent to "delete everything and start again".
