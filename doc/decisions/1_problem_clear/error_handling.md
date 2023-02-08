# Error Handling

## Problem

There are ambiguous cases where the same return value can refer to multiple problems:

- name modifications which can be either invalid name or locking the key name
- getting values of (non-)binary keys

## Constraints

We also have to consider that many parts of the API very constrained in what values they can use for error codes. If a function returns a pointer the only possible error value by definition is NULL.

## Assumptions

## Considered Alternatives

- Change return types for many functions across the API

## Decision

- Update documentation in `doc/dev/error-*` and link to them in the documentation for the module `kdb`
- Add second channel for getting information about errors
- Return error codes directly from functions where failures are expected, e.g. `kdbGet`, `keySetName`
- Harmonize return values from all functions and move error reporting to second channel

## Rationale

This simplifies and unifies error reporting across all functions.
It also introduces a default way for the developers to handle error reporting.
Developers of bindings for Elektra can rely on the existing error reporting feature without having to introduce custom error messages, that can vary wildly between different bindings.

## Implications

## Related Decisions

- Binary metadata vs flag #4194

## Notes
