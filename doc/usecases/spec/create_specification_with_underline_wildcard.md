# Use Case: Create specification with wildcard `_` for an application

## Summary

- **Scope:** `spec`
- **Level:** Developer Goal
- **Actors:** Application Developer
- **Brief:** An application using multiple `mail server` and using wildcard character `_` to give possibility for naming `mail servers`.

## Scenarios

- **Precondition:** None.
- **Main success scenario:**
  - Using `_` wildcard to give `mail server` a name instead of [Array specification](/doc/usecases/spec/create_array_specification.md).
  - Using `mailserver/_/port`, `mailserver/_/hostname` and `mailserver/_/protocol`.
- **Alternative scenario:**
  - Using [Array specification](/doc/usecases/spec/create_array_specification.md) and introducing another key `mailserver/#/name`.
- **Error scenario:**
  - Wrong metakeys are used (yielded as error to the user)
- **Postcondition:** None.
- **Non-functional Constraints:** None.

## Example

This example illustrates the use of an application which uses multiple `mail servers` and using the wildcard character `_`.

A specification for this use case can be like this:

```ni
[mailserver/_/port]
meta:/type = unsigned_short
meta:/description = "The port for the mail server."

[mailserver/_/hostname]
meta:/type = string
meta:/description = "The hostname for the mail server."

[mailserver/_/protocol]
meta:/type = string
meta:/description = "The protocol used by the mail server."
```

Now one could define keys like this:

```ni
mailserver/mail1/port = 25
mailserver/mail1/hostname = domain1.test
mailserver/mail1/protocol = smtp

mailserver/mail2/port = 25
mailserver/mail2/hostname = domain2.test
mailserver/mail2/protocol = smtp
```
