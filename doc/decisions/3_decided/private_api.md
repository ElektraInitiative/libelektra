# Private API

## Problem

Only `libelektra-core` is supposed to access private data, but this contradicts the goal to keep the library minimal.
`kdbprivate.h` was too generic, it contained many other parts next to the struct definitions of Key/KeySet.
Theoretically everything in `kdbprivate.h` is supposed to be private, but lots of code still uses it when it shouldn't.
`kdb.h` is also the only header that is definitely public.
All other headers are anybody's guess.

## Constraints

- The [C99 standard, section 5.2.4.1](https://www.open-std.org/jtc1/sc22/wg14/) gives following limit:
  4095 external identifiers in one translation unit

## Assumptions

## Considered Alternatives

- keep current situation, as described above

## Decision

- Also allow other libraries (e.g. a new `libelektra-operations`) to access to non-public API.
  Such libraries need to have a good reason (e.g. performance, impossible otherwise, etc.) why they access non-public API and they need to be kept up-to-date.
  If a library cannot provide a stable API on top of the unstable non-public API, it clearly needs to state which APIs are not stable.
- Put struct definitions of Key/KeySet in a separate header file, which gets included by parts that need it (see also [Header File Structure](../0_drafts/header_file_structure.md)).

## Rationale

- allows various users (plugins, applications) to link to (more or less) exactly what they need
- allows alternative implementation of parts of Elektra, e.g. a libcore written in Rust
- facilitates code reuse between plugins
- The `extern const` version constant(s) can be used for compatibility checks.
  In most cases, compatibility should be ensured via package management systems or manually by the user.
  Sometimes it may be possible to write code (e.g. in a third-party library) such that it is compatible with multiple versions of Elektra.
  In those cases, we need to know what version of Elektra is installed, so that the correct code can be executed.
  For example, if a new version accepts an argument to `foo()` that previously wasn't allowed.
  An external library may be able to use this when available and use other code as a fallback in older versions.

## Implications

- Any library that uses non-public API, but itself exposes a public API, must either
  - hide breaking changes of the underlying non-public API
  - increment it's major version, if an underlying non-public API causes an incompatible change
- For some libraries (e.g. `libelektra-operations`) it might be easiest to require a specific version of `libelektra-core`. This cannot easily be enforced, so it must be well documented.
- Plugins can safely be used as before, but loading may fail if there is a version mismatch (only important for third-party plugins).

## Related Decisions

- [Header File Structure](../0_drafts/header_file_structure.md)

## Notes
