# Library Split

## Problem

Only libelektra-core is supposed to access private data but this contradicts the goal to keep the library minimal.
`kdbprivate.h` was too generic, it contained many other parts next to the struct definitions of Key/KeySet.

## Constraints

- The [C99 standard, section 5.2.4.1](http://www.open-std.org/jtc1/sc22/wg14/) gives following limit:
  4095 external identifiers in one translation unit
- Some parts of Elektra, like `mmapstorage` need access to private data structure.
- Elektra does not support several different struct definitions of Key/KeySet.
  Alternative implementations that want to coexist (e.g. `mmapstorage` should still work)
  must use the same struct definitions of Key/KeySet.

## Assumptions

## Considered Alternatives

- keep current situation

## Decision

Also allow `libelektra-operations` library to access private Key/KeySet.
Put struct definitions of Key/KeySet in a separate header file, which gets
included by parts that need it

## Rationale

- allows various users (plugins, applications) to link to (more or less) exactly what they need
- allows symbol versioning on different levels (for different evolving libraries)
- allows alternative implementation of parts of Elektra, e.g. a libcore written in Rust
- facilitates code reuse between plugins

## Implications

- we need to clearly communicate which plugins/libraries must be exactly in the version of the libelektra-core

## Related Decisions

- none

## Notes
