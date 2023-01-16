# Use Case: Create array specification for an application

## Summary

- **Scope:** `spec`
- **Level:** Developer Goal
- **Actors:** Application Developer
- **Brief:** This use case introduces the example of array specification with an application configuring multiple mail accounts.

## Scenarios

- **Precondition:** None.
- **Main success scenario:**
  - The developer wants to extend their application from ["Writing specification for an application"](/doc/usecases/plugins/spec/create_specification.md) to support multiple mail accounts.
  - The configuration uses `mailserver/#/port`, `mailserver/#0/hostname` and `mailserver/#0/protocol`.
  - The array is defined by the wildcard character `#`.
  - The keys are all stored for the `spec` namespace.
- **Alternative scenario:** None.
- **Error scenario:**
  - Wrong metakeys are used (yielded as error to the user)
- **Postcondition:** None.
- **Non-functional Constraints:** None.

## Example

This example illustrates the use of an application which uses multiple `mail servers`.

A specification for this use case can be like this:

```ni
[mailserver/port]
meta:/type = unsigned_short
meta:/description = "The port for the mail server."

[mailserver/hostname]
meta:/type = string
meta:/description = "The hostname for the mail server."

[mailserver/protocol]
meta:/type = string
meta:/description = "The protocol used by the mail server."

[mailserver2/port]
meta:/type = unsigned_short
meta:/description = "The port for the mail server."

[mailserver2/hostname]
meta:/type = string
meta:/description = "The hostname for the mail server."

[mailserver2/protocol]
meta:/type = string
meta:/description = "The protocol used by the mail server."
```

One can examine that using this approach is very error-prone.

A solution to this is `array specification` in Elektra with the wildcard character `#`.
A specification using `array specification` can look like this:

```ni
[mailserver/#/port]
meta:/type = unsigned_short
meta:/description = "The port for the mail server."

[mailserver/#0/hostname]
meta:/type = string
meta:/description = "The hostname for the mail server."

[mailserver/#0/protocol]
meta:/type = string
meta:/description = "The protocol used by the mail server."
```

Now one could define keys like this:

```ni
mailserver/#0/port = 25
mailserver/#0/hostname = domain1.test
mailserver/#0/protocol = smtp

mailserver/#1/port = 25
mailserver/#1/hostname = domain2.test
mailserver/#1/protocol = smtp
```
