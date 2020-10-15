# Base Name

## Problem

Both applications and configuration file formats might have arbitrary strings which need to be
encoded within a key name part.

For example:

- an application uses names of internal components as sections within the configuration.
- a parser reads an empty string, to be encoded as base name.

## Constraints

- `keySetBaseName (key, keyBaseName (key))` should be a NOP
- support any configuration file format (i.e., any valid file of some format can be transformed to a KeySet,
  e.g. TOML supports empty key part names).

## Assumptions

## Considered Alternatives

- restrict what `keySetBaseName` can accept has the downside that:
  - applications would suddenly fail when trying to set some key base names
  -

## Decision

`keySetBaseName` must be able to encode any string as base name.

## Rationale

- hard to use it wrong API: having only a single function `keySetBaseName`

## Implications

## Related Decisions

- [Characters](characters.md)

## Notes
