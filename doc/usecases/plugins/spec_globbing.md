# Use Case: Specification for application communicating with multiple services

## Summary

- **Scope:** Configuration
- **Level:** Developer Goal
- **Actors:** Application developer
- **Brief:** The user wants to specify the port and url for an unknown number of servers, which the application shall use.

## Scenarios

- **Precondition:** kdb is installed.
- **Main success scenario:**
  - Specification is written and stored system-wide or user specific.
  - All services which are added in the configuration need to adhere to this configuration specification.
- **Alternative scenario:** None.
- **Error scenario:**
  - In case services have overlapping configuration an error is yielded.
  - A sample for such overlapping would be `service/#/port` and `service/_/port`.
  - The user is informed about the problem / error.
- **Postcondition:** The specification allows configuration for multiple services.
- **Non-functional Constraints:** None.

## Example

This configuration allows the user to configure multiple services used by an application.
It is generic so any service can be configured as dependency e.g. database, message broker, etc.

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

**Values:**

```ni
service/database/port = 5432
service/database/url = jdbc:mysql://localhost

service/activemq/port = 61616
service/activemq/url = tcp://localhost
```
