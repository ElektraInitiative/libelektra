# Private API

## Problem

Only `libelektra-core` is supposed to access private data but this contradicts the goal to keep the library minimal.
`kdbprivate.h` was too generic, it contained many other parts next to the struct definitions of Key/KeySet.

## Constraints

## Assumptions

- The [C99 standard, section 5.2.4.1](http://www.open-std.org/jtc1/sc22/wg14/) gives following limit:
  4095 external identifiers in one translation unit

## Considered Alternatives

- keep current situation

## Decision

- Also allow other libraries (e.g. a new `libelektra-operations`) to access to non-public API.
- Put struct definitions of Key/KeySet in a separate header file, which gets included by parts that need it (see also [Header File Structure](header_file_structure.md)).
- Define the current API version of the core API as
  ```c
  extern const int ELEKTRA_CORE_VERSION_MAJOR;
  extern const int ELEKTRA_CORE_VERSION_MINOR;
  extern const int ELEKTRA_CORE_VERSION_PATCH;
  ```
  in the public API.
- Plugins can define compatible ranges of core API version via the contract. This version is checked during `elektraPluginOpen` by `libelektra-kdb`.

## Rationale

- allows various users (plugins, applications) to link to (more or less) exactly what they need
- allows separate versioning for every library
- allows alternative implementation of parts of Elektra, e.g. a libcore written in Rust
- facilitates code reuse between plugins
- The `extern const int` API version can be used for compatibility checks.

## Implications

- Any library that uses non-public API, but itself exposes a public API, must either
  - hide breaking changes of the underlying non-public API
  - increment it's major version, if an underlying non-public API causes an incompatible change
- For some libraries (e.g. `libelektra-operations`) it might be easiest to require a specific version of `libelektra-core`. This cannot easily be enforced, so it must be well documented.
- Plugins can safely be used as before, but loading may fail if there is version mismatch.

## Related Decisions

- [Header File Structure](header_file_structure.md)

## Notes
