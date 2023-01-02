# Use Case: Specification for REST application communicating with multiple services

## Summary

- **Title:** Specification for REST application communicating with multiple services
- **Scope:** Configuration
- **Level:** Developer Goal
- **Actors:** User (usually a application developer)
- **Brief:** User configures specifcation for configuration of REST application.

## Scenarios

- **Precondition:** kdb is installed.
- **Main success scenario:** Specification is written and stored system wide or user specifc. All services which are added in the configuration need to adhere to this configuration specification.
- **Alternative scenario:** User is limited to only using one service if globbing is not used. 
- **Error scenario:** Technical problems while writing the specification.
  The user is informed about the problem.
- **Postcondition:** The specification allows configuration for multiple services.
- **Non-functional Constraints:** -

## Example

This configuration allows the user to configure multiple services used by the REST application.
It is generic so any service can be configured as dependency e.g. database, message broker, etc.

**Specification:**
```ni
[service/_/port]
meta:/description = Port of an external service used by the REST application.
meta:/type = unsigned_short
meta:/example = 8080
meta:/default = 8080

[service/_/url]
meta:/description = Url of an external service used by the REST application.
meta:/type = string
```

**Values:**
```ni
service/database/port = 5432
service/database/url = jdbc:mysql://localhost

service/activemq/port = 61616
service/activemq/url = tcp://localhost
```

