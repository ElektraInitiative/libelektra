# Lowlevel Library

## Problem

There are many functions that can be useful, but are not required to form the minimal API.
Since we want to keep the API of `libelektra-core` minimal, we cannot add these functions.

If a function implements functionality that is useful in and of itself, it should be added to [`libelektra-operations`](operations_library.md).
In other words: A function that is useful, because it simplifies some general interaction with Elektra.
In contrast to those functions, there are some functions that are only useful as a C API and shouldn't be part of a language binding.
These functions shouldn't be part of `libelektra-operations`.

## Constraints

- `libelektra-core` should have a minimal (but still useful) API.
- `libelektra-operations` should contain generally useful, but not minimal API, that can be used via language bindings

## Assumptions

- There aren't too many of these C-specific functions for a single library.
  But: If the library grows to big, we can still split is apart.

## Considered Alternatives

- Put everything into `libelektra-operations`

## Decision

Create a new `libelektra-lowlevel-c` library (as a counterpart to `libelektra-highlevel`).
This library provides functions which are:

- not minimal
- useful to C code
- but not useful or not usable outside of C code

The implementations should still be as efficient as possible and will therefore access private API directly.

`libelektra-lowlevel-c` depends on `libelektra-operations` and therefore on `libelektra-core`.

As an example for what should be part of this library take a look at [`keyGetNextPart`](iterating_name_parts.md.md).
It can be useful to C code (especially for beginners), but it clearly shouldn't be used in any language that has its own iterator type.

Other examples include `elektraKeyCreate` and `elektraKsCreate`.
Their API allows the creation of an entire KeySet within a single expression.
However, the API was specially designed for use in C and is very hard or impossible to use from other languages.
This makes them the ideal candidates for `libelektra-lowlevel-c`.

## Rationale

The separation between `libelektra-operations` and `libelektra-lowlevel-c` has a few advantages:

- It is clear what the target audience for the API is (everyone vs. C-only)
- Binding authors now that they should (in most cases) creates bindings for everything in `libelektra-operations`.
  If we just had one library, binding authors would have to check every function.
- Because `libelektra-lowlevel-c` targets C exclusively, it can make heavy use of macros and other special features, if needed.

## Implications

`libelektra-lowlevel-c` depends on `libelektra-operations` and `libelektra-core`, so `libelektra-core` and `libelektra-operations` cannot use functions from `libelektra-lowlevel-c`.
Anything that is needed in `libelektra-core`/`libelektra-operations` and should be public must be implemented there.

## Related Decisions

- [Iterating Keyname Parts](iterating_name_parts.md)

## Notes
