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

## Decision

Remove the escaped name from `struct _Key` and use it only when necessary.

Clarify and reduce [terminology](/doc/help/elektra-glossary.md).

API Changes:

- `keyName` returns the unescaped name
  (temporary some other name for PR: `keyUnescapedName(Size)`)
- remove `keyUnescapedName`, `keyGetUnescapedNameSize`.
- reverse terminology: with "key name" we will refer to the unescaped (base) name,
  the escaped name will be explicitly called "escaped key name".
- escaped name will be only present in
  - `keyNew` (+ arguments for adding key names) [unclear: maybe not needed]
  - `elektraEscapeName` (operating on chars)
  - rename `keyAddName`, e.g. to `keyAddEscapedName`
- `keyDup(.., int)` with options to filter which parts are copied
  (to allow copy of keys where only the key name is copied)

## Rationale

## Implications

- needs fix of OPMPHM
- Later modifications in keyNew to also accept key base names might be useful:

- keyNew(KEY_VALUE, "...", KEY_NAME, "", "...", KEY_END)
- keyNew (KEY_NAME, KEY_NS_USER, "abc", "def", KEY_NAME_END, KEY_END);

## Related Decisions

## Notes
