# Use Case: Writing specification for an application

## Summary

- **Scope:** `spec`
- **Level:** User Goal
- **Actors:** User
- **Brief:**
  - This use case shows a sample for writing a specification.
  - It focuses on the specification language rather than on `kdb commands`.

## Scenarios

- **Precondition:** None.
- **Main success scenario:**
  - The application is an email client.
  - It needs to know about the hostname, protocol and port of the user's email account.
  - The application uses the config keys `mailserver/hostname`, `mailserver/protocol` and `mailserver/port`, to let the user configure it's account.
  - Config is written to the `spec` namespace.
  - The parent key for this config keys is `/sw/organization/mailclient/#0/production`.
- **Alternative scenario:** None.
- **Error scenario:**
  - User is informed about problem / error when kdb tool is used wrongly.
- **Postcondition:** None.
- **Non-functional Constraints:** None.

## Example

This use case describes the main purpose of the specification language.
It is about how to write a specification for an application.

In this example we will be using a simple specification for an email client application.
The REST application uses only one service (`mail server`).
The configuration requirements for this REST application are:

- mail server port
- mail server hostname
- mail server protocol

> NOTE: All keys are created with a prepended namespace `spec:/`.
> Keys prepended with `meta:/` are metadata.

A specification for this requirements can be like this (ni format is used here):

```ni
[mailserver/port]
meta:/type = unsigned_short
meta:/description = "The port of the mail server."

[mailserver/hostname]
meta:/type = string
meta:/description = "The hostname of the mail server."

[mailserver/protocol]
meta:/type = string
meta:/description = "The protocol the mail server uses."
```

The parent key for this specification is `/sw/organization/mailclient/#0/production`.
Metakeys are used to give more details about the keys in the specification.
In this example `type` and `description` are used.
