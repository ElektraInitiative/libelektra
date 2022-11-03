# Base Name

## Problem

A key name is made out of a sequence of key part names, and can be constructed with `keyAddBaseName/keySetBaseName`.
Both applications and configuration file formats might need arbitrary strings to be encoded within a key name part.

For example:

- an application uses names of internal components as sections within the configuration.
- a parser reads an empty string, to be encoded as base name.

## Constraints

- `keySetBaseName (key, keyBaseName (key))` should be a NOP,
  which is needed for round-trips: If a storage plugin serializes what it gets with `keyBaseName`;
  `keySetBaseName` must lead to the same key.
- support any configuration file format (i.e., any valid file of some format can be transformed to a KeySet,
  e.g. TOML supports empty key part names).

## Assumptions

## Considered Alternatives

- restrict what `keyAddBaseName/keySetBaseName` can accept: has the downside that
  applications would suddenly fail when trying to set some key base names
- have additional `keySetBaseName*` functions that make strings safe to be accepted
  in `keyAddBaseName/keySetBaseName`: seems to be a too big change in the storage plugins

## Decision

`keyAddBaseName/keySetBaseName` never fail with any argument, so any character sequence can be escaped except of NULL bytes.
The argument goes unmodified to the unescaped key name.

For arrays there is no escaping needed because an array is only an array if the metadata `array` is appended to the direct parent key.
See [array](../4_partially_implemented/array.md).

## Rationale

- hard to use it wrong API: having only the functions `keyAddBaseName/keySetBaseName`, without any size argument
- applications and storage plugins can pass any C string to `keyAddBaseName/keySetBaseName` without any further consideration

## Implications

## Related Decisions

- [Array](../4_partially_implemented/array.md)

## Notes
