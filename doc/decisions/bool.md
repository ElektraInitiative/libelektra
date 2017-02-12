# Definition of Bool

## Issue

Inconsistent use of bool in various parts of Elektra.

## Constraints

## Assumptions

- needs to be string
- convenience plugins can convert anything to 0 or 1
- type checker plugins can reject everything not 0 or 1

## Considered Alternatives

- strictly only allow 0 and 1 (would move validation across the code)
- only check presence or absence (no cascading override of already present key possible)
- use as in CMake (would move convenience across the code)

## Decision

Use, depending on what your default should be:

- 0 is false, everything else is true (default is true)
- 1 is true, everything else is false (default is false)

Example:

    if ( strcmp(keyString(k), "0")) {/*true*/} else {/*false*/}
    if (!strcmp(keyString(k), "1")) {/*true*/} else {/*false*/}

In the documentation it should mention that a bool is used
and which is the default.

The type checker plugin should allow

- non-presence (default)
- the string "0"
- the string "1"

The convenience plugin should transform (it might be combined with a plugin that transforms everything lower-case):

- "false", "off", "no"  to "0"
- "true",  "on",  "yes" to "1"

## Argument

- most easy to implement
- allows presence to be true
- plugins allow us to convert to any other behaviour

## Implications

- change code with different behavior

## Related decisions

## Notes

See [here](https://github.com/ElektraInitiative/libelektra/issues/308)
