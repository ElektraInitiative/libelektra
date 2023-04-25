# Use Case: Create simple specification for dockerd configuration file (daemon.json)

## Summary

- **Scope:** `spec`
- **Level:** Developer Goal
- **Actors:** Dev-Ops Engineer
- **Brief:** This use case introduces a simple specification for a part of the dockerd configuration file (daemon.json).

## Scenarios

- **Precondition:** The Dev-Ops Engineer has a understanding of `docker` and the daemon `dockerd`.
- **Main success scenario:**
  - The Dev-Ops Engineer wants to write a specification for the `default runtime` in the `dockerd` configuration.
  - The configuration key is `default/runtime`.
  - The configuration key uses `type`, `description` and `default` as metakeys.
  - The keys are all stored for the `spec` namespace.
- **Alternative scenario:** None.
- **Error scenario:**
  - Wrong metakeys are used (yielded as error to the user).
- **Postcondition:** The keys are all stored for the `spec` namespace..
- **Non-functional Constraints:** None.

## Example

The `default runtime` configuration for the `dockerd` could look like:

```ini
[default/runtime]
meta:/type = string
meta:/description = Default OCI runtime for containers
meta:/default = runc
```

In case the key `default/runtime` does not exist, `spec` plugin creates a `default` key with value `runc` in the default namespace.

For the full specification of the `dockerd` configuration file see [dockerd-spec](dockerd.spec).
