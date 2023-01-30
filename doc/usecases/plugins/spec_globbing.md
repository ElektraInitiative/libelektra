# Use Case: Specification using Globbing

## Summary

- **Scope:** Configuration
- **Level:** Developer Goal
- **Actors:** Application developer
- **Brief:** Writing generic specification using elektra globbing in key names.

## Scenarios

- **Precondition:** kdb is installed.
- **Main success scenario:**
  - Globbing is used in order to make specification generic.
  - Globbing can be achieved by using wildcard characters.
  - The used wildcard characters here are `_` and `#`.
  - In order to write the specification, the kdb tool is used.
  - To create a key, meta-keys are created immediately as well.
  - This command `kdb meta-set spec:/service/_/port type string` creates a key `service/_/port` with metadata `type` indicating the `port` 
  is of type string.
  - `service/_/port` allows us to create any key name starting with `service/` and ending with `/port`.
  - A possible example is `service/mailserver/port`.
  - The usage of this wildcard character `_` is called `globbing`.
- **Alternative scenario:** None.
- **Error scenario:**
  - In case services have overlapping specification an error is yielded.
  - A sample for such overlapping would be `service/#/port` and `service/_/port`.
  - The user is informed about the problem / error.
- **Postcondition:** 
  - The specification allows assigning key names of starting with `service/` and ending with `/port`.
  - The specification makes sure only `string` is used as `port` type.
- **Non-functional Constraints:** None.

## Example

This configuration allows the user to configure multiple services used by an application.
It is generic so any service can be configured as dependency, in the example below database and activemq:

**Specification:**

```ni
[service/_/port]
meta:/description = Port of an external service used by an application.
meta:/type = unsigned_short
meta:/example = 8080
meta:/default = 8080

[service/_/url]
meta:/description = Url of an external service used by an application.
meta:/type = string
```

**Configuration:**

```ni
service/database/port = 5432
service/database/url = jdbc:mysql://localhost

service/activemq/port = 61616
service/activemq/url = tcp://localhost
```
