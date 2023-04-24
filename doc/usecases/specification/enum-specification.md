# Use Case: Create enum specification for dockerd configuration file (daemon.json)

## Summary

- **Scope:** `spec`
- **Level:** Developer Goal
- **Actors:** Dev-Ops Engineer
- **Brief:** This use case introduces a enum specification for a part of the dockerd configuration file (daemon.json).

## Scenarios

- **Precondition:** None.
- **Main success scenario:**
    - The Dev-Ops Engineer wants to write a specification for the `log level` in the `dockerd` configuration.
    - The configuration key is `log/level`.
    - The configuration key uses `description`, `default` and `enum` as metakeys.
    - The specification strictly defines all enum values `debug`, `info`, `warn`, `error` and `fatal`.
    - The keys are all stored for the `spec` namespace.
- **Alternative scenario:** None.
- **Error scenario:**
    - Wrong metakeys are used (yielded as error to the user).
- **Postcondition:** None.
- **Non-functional Constraints:** None.

## Example

The `log level` configuration for the `dockerd` could look like:

```ini
[log/level]
meta:/description = Set the logging level
meta:/enum/check = #4
meta:/enum/check/#0 = debug
meta:/enum/check/#1 = info
meta:/enum/check/#2 = warn
meta:/enum/check/#3 = error
meta:/enum/check/#1 = fatal
meta:/default = info
```

In case the key `log/level` does not exist, `spec` plugin creates a `default` key with value `info` in the default namespace.

For the full specification of the `dockerd` configuration file see [dockerd-spec](dockerd.spec).
