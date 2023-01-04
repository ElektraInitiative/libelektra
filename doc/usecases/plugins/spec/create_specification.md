# Use Case: Writing specification for an application

## Summary

- **Scope:** `spec`
- **Level:** Developer Goal
- **Actors:** Application, User
- **Brief:** 
    - Creating a specification for a mail client application which needs communication to a mail server.

## Scenarios

- **Precondition:** kdb is installed.
- **Main success scenario:** 
    - Specification for mail server application is available.
    - Specification is written user or system wide.
    - Mail client uses configuration for only one mail server.
- **Alternative scenario:** None.
- **Error scenario:**
    - User is informed about problem / error when kdb tool is used wrongly.
- **Postcondition:** None.
- **Non-functional Constraints:** None.

## Example

This use case describes the main purpose of the specification language.
It is is to write a specification for an application.

In this example we will be using a simple specification for an email client application.
The REST application uses only one service (`mail server`).
The configuration requirements for this REST application are:
- mail server port
- mail server hostname
- mail server protocol

A specification for this requirements can be like this (ni format is used here):

```ni
[service/mailserver/port]
meta:/type = unsigned_short
meta:/description = "The port of the mail server."

[service/mailserver/hostname]
meta:/type = string
meta:/description = "The hostname of the mail server."

[service/mailserver/protocol]
meta:/type = string
meta:/description = "The protocol the mail server uses."
```

The parent key for this specification is `/sw/organization/mailclient/#0/production`.
Metakeys are used to give more details about the keys in the specification.
In this example `type` and `description` are used.