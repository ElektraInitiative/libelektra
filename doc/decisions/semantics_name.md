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

There are, however, rules and conventions which syntax to use for specific semantics.
The `spec` plugin guards these rules.

## Rationale

## Implications

## Related Decisions

- [Arrays](array.md)

## Notes
