# Use Case: Create specification with underline wildcard for an application

## Summary

- **Scope:** `spec`
- **Level:** Developer Goal
- **Actors:** Application, User
- **Brief:** 
    - Creating a specification for a REST application which needs communication to multiple services.

## Scenarios

- **Precondition:** kdb is installed.
- **Main success scenario:** 
    - Specification is written user or system wide.
    - REST application can use any number of services.
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

A solution to this besides `array specification` in Elektra is with the wildcard character `_`.
A specification using `_` can look like this:

```ni
[service/_/port]
meta:/type = unsigned_short
meta:/description = "The port for the service."

[service/_/url]
meta:/type = string
meta:/description = "The url for the service."
```

Now we can define our keys for our two services as:
```ni
service/mailserver/port = 587
service/mailserver/url = tcp://localhost

service/messagebroker/port = 61616
service/messagebroker/url = tcp://localhost
```

We can see that this kind of specification is much better when it commes to having many services and similar configuration for the keys.

To see another solution take a look at [create_array_specification](create_array_specification.md).