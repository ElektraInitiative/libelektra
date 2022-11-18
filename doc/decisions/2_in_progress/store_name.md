# Store the escaped and/or unescaped key name

## Problem

Memory Consumption in Elektra is quite high as the key names are long and stored twice in memory.

## Constraints

- comparing of keys and thus searching in key sets must be possible with memcmp

## Assumptions

- printing the escaped key name is not so much used in typical applications and in tooling doing extra escaping does not matter

## Considered Alternatives

- store both
- store both but in one memory block (previous implementation)
- only store escaped name

## Decision

Only store the unescaped key name, suitable for comparing/searching/iterating over name, i.e.:

- Remove the escaped name from `struct _Key` and use it only when necessary.
- Clarify and reduce [terminology](/doc/help/elektra-glossary.md).
- API Changes:
  - `keyNew (const char*, size_t)`
  - `keyName` returns the unescaped name
  - remove `keyUnescapedName`, `keyGetUnescapedNameSize`.
  - reverse terminology: with "key name" we will refer to the unescaped (base) name,
    the escaped name will be explicitly called "escaped key name".
  - escaped name will be outside the core for tooling
  - `keyDup(.., int)` with options to filter which parts are copied
    (to allow copy of keys where only the key name is copied)

## Rationale

- saves memory
- reduces API (`keyUnescapedName*`)

## Implications

- users should use compare functions, .. instead of `keyName`
- modifications in keyNew needed

## Related Decisions

## Notes
