# Operations Library

## Problem

There are many functions that can be useful, but are not required to form the minimal API.
Since we want to keep the API of `libelektra-core` minimal, we cannot add these functions.

## Constraints

- `libelektra-core` should have a minimal API.

## Assumptions

- The number of functions for the new library is not high enough to warrant a split into multiple languages.
  (see also Rationale below)

## Considered Alternatives

## Decision

Create a new `libelektra-operations` library.
This library provides generally useful and performant, but not minimal, functions.
The implementations should still be as efficient as possible and will therefore access private API directly.

`libelektra-operations` only depends on `libelektra-core`.

Move basic, but not minimal, functions from `libelektra-core` to `libelektra-operations`:

- `keyNeedSync`
- `keyGetRef`
- `ksDeepDup`
- `ksGetAlloc`
- `ksGetRef`

Also add other useful functions:

- [`keyIsBelow`](key_below.md)

The functions in `libelektra-operations` should be designed in such a way that creating bindings for other languages is possible.
In particular, variadic arguments should be avoided.

## Rationale

If the library grows to big, we can still split it into multiple libraries in future.
By keeping a "dummy library" that links to the split libraries, this can be done in a backwards compatible way.

However, such a split should only be a last resort.
We must always consider creating a new separate library, _before_ adding new functions to `libelektra-operations`.
For example, very specialized libraries like `libelektra-opts` and `libelektra-merge` should always remain separate.
They are only needed in specific circumstances and not every application should need to load them.
In contrast, some parts of `libelektra-ease` (e.g. type conversion) may very well be moved to `libelektra-operations`.

## Implications

`libelektra-operations` depends on `libelektra-core`, so `libelektra-core` cannot use functions from `libelektra-operations`.
Anything that is needed in `libelektra-core` and should be public must be implemented there.

## Related Decisions

- [`keyIsBelow`](key_below.md)

## Notes
