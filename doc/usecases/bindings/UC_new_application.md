# Use Case: Programming Language Bindings

## Summary

- **Scope:** API
- **Level:** Sea
- **Actors:** API User
- **Brief:** Developer wants to use libelektra in a new application written in another language than C
- **Status:** Draft

## Scenarios

- **Precondition:** A binding for the programming language must exist and the maintainers of the new application decided to use libelektra.
- **Main success scenario:** The application can be fully configured through libelektra
- **Alternative scenario:** Due to limitations of certain bindings, not everything but the majority can be configured
- **Error scenario:** The application might be put into a faulty state when the keys get modified by another application or manually
- **Postcondition:** The new application is able to read and write with libelektra
