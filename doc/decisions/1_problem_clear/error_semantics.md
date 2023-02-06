# Error Semantics

## Problem

While we have a classification of errors and warnings, it remains unclear when plugins actually should emit errors and warnings.

## Constraints

- Should not be contradicting to specified behavior in [storage plugin tutorial](/doc/tutorials/storage-plugins.md).

## Assumptions

- Users want a uniform behavior within Elektra, so plugins must behave uniformly.

## Considered Alternatives

- freedom to plugin writers
- strict rules and conformance tests for plugins

## Decision

Provide guidelines in the form as tutorials, covering:

- prefer errors to warnings
- that any not understood metadata (e.g. types), should lead to an error
- that wrong specifications, like `kdb meta-set /tests/ipaddr/ipv4 check/ipaddr ipv8` should be rejected
- if the value does not confirm **exactly** to the specified type, an error should be emitted
  (e.g. only `0` or `1` as boolean)
- anything else that is beyond the capabilities of a plugin (not implemented), should lead to an error

Violations against these guidelines can be reported as bug and then either:

- the bug gets fixed
- the plugin get a worse `infos/status` but still get shipped with 1.0
- the plugin gets removed

## Rationale

It is easier for developers if there are clear expectations on how a plugin should behave.
And it is much easier for overall Elektra if there is more consistency.

## Implications

- more checks&errors in storage plugins are needed

## Related Decisions

- [Metadata in Spec Namespace](spec_metadata.md)
- [Capabilities](../4_decided/capabilities.md)
- [Boolean](../4_partially_implemented/boolean.md)

## Notes

- [Issue #1511](https://issues.libelektra.org/1511)
