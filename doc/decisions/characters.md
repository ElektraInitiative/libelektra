# Characters

## Problem

Users must be safe from interpretations of plugins.
`keySetBaseName("@BASE64abc")` should not suddenly turn the name to something else (given a base64 plugin for names).
Nor `keySetBaseName("#123")` should suddenly turn some keys to an array.

It is not possible to allow all of following:

- have any characters in key name parts for applications
- being able to encode information in names

## Constraints

## Assumptions

## Considered Alternatives

## Decision

Decide in favour of key name parts with any characters.

For arrays there is actually not an conflict because an array is only an array if the meta-data `array` is appended to the direct parent key.
See [array](array.md). So `#` does not need to be escaped.

For extensibility of Elektra, however, at least one character needs a special meaning.
We will use `@` for that.
So `keySetBaseName(k, "@BASE64abc")` will internally escape `@`.

## Rationale

## Implications

## Related Decisions

- [Base Name](base_name.md)

## Notes
