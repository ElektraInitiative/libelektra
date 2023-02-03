# Error Semantics

## Problem

While we have a classification of errors and warnings, it remains unclear when plugins actually should emit errors and warnings.

## Constraints

- Should not be contradicting to specified behavior in [storage plugin tutorial](/doc/tutorials/storage-plugins.md).

## Assumptions

- Users want a uniform behavior within Elektra, so plugins must behave uniformly.

## Considered Alternatives

### Free-for-all

Every plugin author decides for themselves.

Leads to inconsistent behavior and hard to diagnose interaction issues.

### Detailed ruleset with conformance tests

Write a detailed formal ruleset and generic conformance tests for all plugins.

Defining the ruleset, let alone writing the tests, for all possible errors in plugins is borderline impossible.

### Simple guidelines

Create simple guidelines for plugin authors and clearly document them.
Rely on plugin authors adhering to the guidelines and testing their plugins themselves.
Plugins that do not adhere to the guidelines can be marked down via `infos/status`.

The basic guidelines will be (but may be extended):

1. During `kdbGet` only emit warnings, unless the error is entirely unrecoverable:
   Whatever other guidelines say, during `kdbGet` plugins shall only emit warnings to allow the caller to fix issues via `kdbSet`.
   If possible, the emitted warnings should be identical to the errors set in `kdbSet()`.
   Only if errors are entirely unrecoverable, an error should be emitted.
   Some examples for either case:

   1. File-based backend cannot parse local file:
      Emit a warning.
      We may still be able to overwrite the file in `kdbSet`.
   2. File-based backend cannot access file, because path is invalid (e.g., on Windows `C:\foo\CON`)
      Emit an error.
      A `kdbSet` could recover, but the mountpoint definition needs to change.
      To do this, a new `kdbOpen` is needed.

2. Prefer errors to warnings in `kdbSet`:
   Most validation detect either valid or invalid values.
   Only in the rare case, where something is valid, but might not be correct a warning should be used.

   For very common mistakes which are still technically valid configuration, both emitting an error and emitting a warning are valid options.
   But in both cases, there should be a way to suppress or avoid the error/warning.
   This suppression could for example be triggered by additional metadata.
   The need for a suppression should be rare, and as such it should be intentionally verbose to suppress errors/warnings.

   An example, of a suppressible warning exists in the `backend` plugin.
   Normally, you get a warning, when you configure a resolver with an absolute path.
   This is based on the assumption that the resolver is there to find the absolute path based on a given relative path.
   However, because sometimes setting an absolute path is necessary, the warning can be suppressed by setting an additional key in the mountpoint configuration.

3. Don't stop after the first error:
   Additional errors, are automatically converted into warnings.
   Report as many errors as possible in one go, so the user can fix as much as possible in one go.

4. Specifications are exact:
   Both the checked config must conform **exactly** to the specification.
   Plugins should not ignore unknown values or values they don't understand.

5. Reject unknowns:
   To be on the safe side, always reject any unknowns that are directed at the plugin.
   Unknown specification should always result in errors.
   Unknown config data may result in warnings instead, unless it is known to be invalid.
   This mostly applies to storage plugins.
   For example, if a plugin handles `meta:/foo`, it should emit an error, if encounters an unknown value for `meta:/foo`.

   Of course, plugins can decide that they are not handling certain keys or metakeys.
   But that should only be based on the keyname, never the value.

6. The `storage` phase should handle everything during `kdbSet`:
   A plugin executed in the `storage` phase of `kdbSet` should store the entire `KeySet` they receive, such that in a future `kdbGet` they will load an equal `KeySet`.
   If such a plugin cannot handle the entire `KeySet` semantics (e.g., no metadata), the unsupported data should be removed in an earlier phase.
   Either by the plugin itself, or by another plugin that is required by said plugin.

   If a plugin does encounter unsupported data during the `storage` phase, it should emit a warning and drop the unsupported data.

## Decision

**Suggestion:** Go with "Simple guidelines"

## Rationale

- Guidelines are needed to avoid chaos
- A strict ruleset is too hard enforce

## Implications

- Plugins need to be checked for guideline conformance

## Related Decisions

- [Metadata in Spec Namespace](../1_in_discussion/spec_metadata.md)
- [Capabilities](../3_decided/capabilities.md)
- [Boolean](../4_partially_implemented/boolean.md)

## Notes

- [Issue #1511](https://issues.libelektra.org/1511)
