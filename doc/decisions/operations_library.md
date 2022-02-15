# Operations Library

## Problem

There are many functions that can be useful, but are not required to form the minimal API.
Since we want to keep the API of `libelektra-core` minimal, we cannot add these functions.

## Constraints

- `libelektra-core` should have a minimal (but still useful) API.

## Assumptions

## Considered Alternatives

## Decision

Create a new `libelektra-operations` library.
This library provides generally useful, but not minimal, functions.
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

## Implications

`libelektra-operations` depends on `libelektra-core`, so `libelektra-core` cannot use functions from `libelektra-operations`.
Anything that is needed in `libelektra-core` and should be public must be implemented there.

## Related Decisions

- [`keyIsBelow`](key_below.md)

## Notes
