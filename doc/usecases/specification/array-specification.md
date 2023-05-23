# Use Case: Create array specification for dockerd configuration file (daemon.json)

## Summary

- **Scope:** `spec`
- **Level:** Developer Goal
- **Actors:** Dev-Ops Engineer
- **Brief:** This use case introduces an array specification for a part of the dockerd configuration file (daemon.json).

## Scenarios

- **Precondition:** The Dev-Ops Engineer has a working setup for `docker` for and the daemon `dockerd`.
- **Main success scenario:**
  - The Dev-Ops Engineer wants to write a specification for the `storage options` in the `dockerd` configuration file.
  - The array specification configuration key is `storage/opts/#`.
  - The configuration uses `array/min` and `description` as metakeys.
  - The array is defined by the wildcard character `#`.
  - The keys are all stored for the `spec` namespace.
- **Alternative scenario:**
  - Define the storage type with an underline specification.
  - The array specification configuration key is `storage/opts/_`.
  - The configuration uses `array/min` and `description` as metakeys.
- **Error scenario:**
  - Wrong metakeys are used (yielded as error to the user).
- **Postcondition:** The keys are all stored for the `spec` namespace.
- **Non-functional Constraints:** None.

## Example

The `storage options` configuration for the `dockerd` could look like:

```ini
[storage/opts/#]
meta:/array/min = 0
meta:/description = Storage driver options
```

For the full specification of the `dockerd` configuration file see [dockerd-spec](../../../examples/spec/dockerd.ini).
