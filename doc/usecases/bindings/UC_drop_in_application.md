# Use Case: Use libelektra as drop-in Replacement

## Summary

- **Scope:** API
- **Level:** Kite
- **Actors:** Administrator
- **Brief:** Administrator wants to use libelektra in an application which uses another configuration management than libelektra
- **Status:** Draft

## Scenarios

- **Precondition:** A binding for the configuration management which the application uses must exist.
- **Main success scenario:** The application can be fully configured through libelektra by using libelektra as a drop-in replacement without changing any source code.
- **Alternative scenario:** Due to limitations of certain bindings, not everything but the majority can be configured.
- **Error scenario:** The application might be put into a faulty state when the keys get modified by another application or manually.
- **Postcondition:** The application is able to read and write through libelektra.
