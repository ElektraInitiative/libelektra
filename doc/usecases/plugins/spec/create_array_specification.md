# Use Case: Create array specification for an application

## Summary

- **Scope:** `spec`
- **Level:** Developer Goal
- **Actors:** Application, User
- **Brief:**
  - Creating a specification for a REST application which needs communication to a mail server and message broker.

## Scenarios

- **Precondition:** kdb is installed.
- **Main success scenario:**
  - Specification is written user or system-wide.
  - REST application can use any number of services
- **Alternative scenario:** None.
- **Error scenario:**
  - Wrong metakeys are used (yielded as error to the user)
- **Postcondition:** None.
- **Non-functional Constraints:** None.

## Example

This example illustrates the use of a REST application which communicates with two services.
The services used are `mail server` and `message broker`.

A specification for this use case can be like this:

```ni
[mailserver/port]
meta:/type = unsigned_short
meta:/description = "The port for the mail server."

[mailserver/url]
meta:/type = string
meta:/description = "The url for the mail server."

[messagebroker/port]
meta:/type = unsigned_short
meta:/description = "The port for the message broker."

[messagebroker/url]
meta:/type = string
meta:/description = "The url for the message broker."
```

We can see that this specification has similar configurations for the services.
Both `url` and `port` are using the same metadata.

This is an example with very few services, but imagine an application with more than 10 services.
We would need to copy and paste the specification which is very error prone and inconvenient.

A solution to this is `array specification` in Elektra with the wildcard character `#`.
A specification using `array specification` can look like this:

```ni
[service/#/port]
meta:/type = unsigned_short
meta:/description = "The port for the service."

[service/#/url]
meta:/type = string
meta:/description = "The url for the service."
```

Now we can define our keys for our two services as:

```ni
service/#0/port = 587
service/#0/url = tcp://localhost

service/#1/port = 61616
service/#1/url = tcp://localhost
```

We can see that this kind of specification is much better when it commes to having many services and similar configuration for the keys.
But we can also identify a shortcoming with this `array specification` approach.
We can no longer no what this services are i.e. if its a `mail server` or `message broker`.

We have two solutions to this problem:

1. Introducing another key `name` which identifies the service.
2. Use another wildcard character `_` (see [create_specification_with_underline_wildcard](create_specification_with_underline_wildcard.md))

The shortcoming with solution 1 is that we need to introduce a new key for just identifying the service.

Solution 1 can look like this:

```ni
[service/#/port]
meta:/type = unsigned_short
meta:/description = "The port for the service."

[service/#/url]
meta:/type = string
meta:/description = "The url for the service."

[service/#/name]
meta:/type = string
meta:/description = "The name for the service."
```
