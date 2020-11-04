# Escaped Name

## Problem

Currently we store both the escaped and unescaped name in a Key and in
a way that there cannot be an optimization not to store it (there are
functions handling out the pointer).

## Constraints

- allow implementations of Elektra to not store two names

## Assumptions

- the memory consumption of storing the name twice is considerable

## Considered Alternatives

- store it twice
- store it once but in one memory block

## Open Points

Clarify and reduce [terminology](/doc/help/elektra-glossary.md):

- escaped name
- full name
- part name
- base name
- dir name

## Decision

- remove the escaped name from `_Key`
- `keyName` returns the unescaped name
- remove `keyUnescapedName`, `keyGetUnescapedNameSize`, `keyGetFullName`.
- reverse terminology: with "key name" we will refer to the unescaped (base) name,
  the escaped name will be explicitly called "escaped key name".
  "full name" (a variant of the escaped name) will not be used anymore.
- escaped name will be only present in `keyNew` and `keyGetEscapedName`
- rename `keyAddName`, e.g. to `keyAddEscapedName`
  (or `keyAddFullName` if we decide to call the escaped name full name)
- `keyDup` with arguments to filter which parts are copied
  (to allow copy of keys where only the key name is copied)

## Rationale

## Implications

- needs fix of OPMPHM
- Later modifications in keyNew to also accept key base names might be useful:

- keyNew(KEY_VALUE, "...", KEY_NAME, "", "...", KEY_END)
- keyNew (KEY_NAME, KEY_NS_USER, "abc", "def", KEY_NAME_END, KEY_END);

## Related Decisions

## Notes
