# Use Case: Create underline specification for dockerd configuration file (daemon.json)

## Summary

- **Scope:** `spec`
- **Level:** Developer Goal
- **Actors:** Dev-Ops Engineer
- **Brief:** This use case introduces an underline specification for a part of the dockerd configuration file (daemon.json).

## Scenarios

- **Precondition:** None.
- **Main success scenario:**
    - The Dev-Ops Engineer wants to write a specification for the `runtimes` in the `dockerd` configuration file.
    - The underline specification configuration keys are `runtimes/_`, `runtimes/_/path`, `runtimes/_/runtimeArgs/#`.
    - The specification uses `type`, `description` and `example` as metakeys.
    - The underline configuration is defined by the wildcard character `_`.
    - The keys are all stored for the `spec` namespace.
- **Alternative scenario:**
    - Define the storage type with an array specification.
    - The array specification configuration keys are `runtimes/#`, `runtimes/#/path`, `runtimes/#/runtimeArgs/#`.
    - The configuration key `runtimes/#` uses `array/min`, `description` and `example` as metakeys.
    - The configuration key `runtimes/#/path` uses `type`, `description` and `example`.
    - The configuration key `runtimes/#/runtimeArgs/#` still uses `array/min`, `description` and `example`.
- **Error scenario:**
    - Wrong metakeys are used (yielded as error to the user).
- **Postcondition:** None.
- **Non-functional Constraints:** None.

## Example

The `runtimes` configuration for the `dockerd` could look like:

```ini
[runtimes/_]
meta:/type = string
meta:/description = Additional OCI compatible runtime
meta:/example = custom

[runtimes/_/path]
meta:/type = string
meta:/description = The path to the OCI compatible runtime
meta:/example = /usr/local/bin/my-runc-replacement

[runtimes/_/runtimeArgs/#]
meta:/array/min = 0
meta:/description = The runtime arguments for the OCI compatible runtime
meta:/example = --debug
```

For the full specification of the `dockerd` configuration file see [dockerd-spec](dockerd.spec).
