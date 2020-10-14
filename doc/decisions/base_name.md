# Base Name

## Problem

Both applications and configuration file formats might have arbitrary strings which need to be
encoded within a key name part.

## Constraints

## Assumptions

## Considered Alternatives

- restrict what `keySetBaseName` can accept is against:
  - easy to use of API: applications with some key names would suddenly fail
  - hard to use it wrong API: storage plugins that do not pre-process key names properly would fail to build up a KeySet

## Decision

`keySetBaseName` must be able to encode any string as base name.

## Rationale

## Implications

## Related Decisions

- [Characters](characters.md)

## Notes
