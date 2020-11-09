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

The spec/docu should mention that a `boolean` is used and may specify the default.
The absence of a non-required key without a specified default should be interpreted as false.

Example for an implementation in C in an application:

```c
if (!strcmp(keyString(k), "1")) {/*true*/} else {/*false*/}
```

Storage plugins are allowed any representation as suitable, e.g., a JSON plugin might render `1` as `true`.

The type checker plugin should allow

- non-presence, if not required
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
