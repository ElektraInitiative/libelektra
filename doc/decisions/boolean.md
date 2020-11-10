# Definition of Bool

## Problem

Inconsistent use of booleans in various parts of Elektra.

## Constraints

- needs to be string

## Assumptions

## Considered Alternatives

- only check presence or absence (no cascading override of already present key possible)
- use booleans as in CMake, which allows on/off, true/false, ... (would need convenience across the code)
- do not accept a specification with `type = boolean` without a default

## Decision

Only the strings `0` and `1` are allowed in the `KeySet` for `type = boolean`, for both values and defaults.
Everything else should lead to errors in checkers (in `kdbSet`).

A spec with `type = boolean` without a specified default should be interpreted as `default = 0`.

Example for an implementation in C in an application:

```c
if (k != NULL && strcmp(keyString(k), "1") == 0) {/*true*/} else {/*false*/}
```

Storage plugins are allowed any representation as suitable, e.g., a JSON plugin might render `1` as `true`.

The type checker plugin should allow

- non-presence
- the string "0"
- the string "1"

## Rationale

- most easy to implement
- allows non-presence to be false
- plugins allow us to convert to any other behavior

## Implications

- Storage plugins are only allowed to emit `0` or `1` as key values
- Applications either get `0` or `1`, or (without a key)
  can safely assume that false is meant

## Related Decisions

## Notes
