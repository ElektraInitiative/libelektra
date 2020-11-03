# Semantics in Key Names

## Problem

It can get quite cumbersome to find out about key interrelations like arrays.

## Constraints

## Assumptions

## Considered Alternatives

The alternative would be to have semantics in key names, with following advantages:

- maybe less metadata to save memory (only array?)

## Decision

Do not encode any semantics into the key names.
All semantic must be in metadata.

Nevertheless, there are guidelines (without any checks in `keySetBaseName`):

- `#` is used to indicate that array numbers follow.
- `®` is used to indicate that some information was encoded in the key name.
  This is usually only needed internally in storage plugins.
- `®elektra` is reserved, key names should not start with that sequence.

There are, however, rules and conventions which syntax to use for specific semantics.
The `spec` plugin guards these rules.

## Rationale

## Implications

## Related Decisions

- [Arrays](array.md)

## Notes
