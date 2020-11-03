# Definition of Bool

## Problem

Inconsistent use of bool in various parts of Elektra.

## Constraints

- needs to be string

## Assumptions

- type checker plugins can reject everything not 0 or 1

## Considered Alternatives

- only check presence or absence (no cascading override of already present key possible)
- use booleans as in CMake, which allows on/off, true/false, ... (would need convenience across the code)

## Decision

Only the strings `0` and `1` are allowed in the `KeySet` for `type = boolean`.
Everything else should lead to errors in checkers (in `kdbSet`).

Storage plugins are allowed any representation as suitable.

In the absence of the key, the default can be either:

- default is true:
  `0` is false, everything else is true, or
- default is false:
  `1` is true, everything else is false.

Example for implementation in C:

```c
if ( strcmp(keyString(k), "0")) {/*true*/} else {/*false*/}
if (!strcmp(keyString(k), "1")) {/*true*/} else {/*false*/}
```

In the spec/docu it should mention that a bool is used
and which is the default.

The type checker plugin should allow

- non-presence (for default), if not required
- the string "0"
- the string "1"

## Rationale

- most easy to implement
- allows presence to be true
- plugins allow us to convert to any other behavior

## Implications

- change code with different behavior

## Related Decisions

## Notes

See [here](https://github.com/ElektraInitiative/libelektra/issues/308)
