# Definition of bool

## Issue

Inconsistent use of bool in various parts of Elektra.

## Constraints

## Assumptions

- needs to be string
- convenience plugins can convert anything to 0 or 1
- type checker plugins can reject everything not 0 or 1

## Considered Alternatives

- 1 is true, everything else is false (does not allow activation by presence, which is currently used)
- strictly only allow 0 and 1 (would move validation across the code)
- only check presence or absence (no cascading override of already present key possible)
- use as in CMake (would move convenience across the code)

## Decision

- 0 is false, everything else is true
- rename options to "noOPTION" to avoid different default behavior

Example:

    if (!strcmp(keyString(k), "0")) {/*false*/}
    else {/*true*/}

## Argument

- most easy to implement
- allows presence to be true
- plugins allow us to convert to any other behaviour

## Implications

- change code with different behavior

## Related decisions

## Notes

See [here](https://github.com/ElektraInitiative/libelektra/issues/308)
