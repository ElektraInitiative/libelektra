# Binary

## Problem

Binary

- is the only metadata that allows more values (instead of making the validation stricter)
- is the only API that modifies metadata
- creates some inconsistencies in the API (e.g. `KEY_FLAG_RO_VALUE` is linked to `KEY_FLAG_RO_META`)

## Constraints

- simplify API

## Assumptions

- byte-array values are a rarely used feature in configuration settings
- absence of values, i.e., [null keys](../6_implemented/null.md) however, is quite common

## Considered Alternatives

- flag for indicating if a value is binary

## Decision

- make keys with untyped value per default (no assumption about type, i.e., byte-array)
- all current types are not binary, so type = string is the way to indicate a string doesn't
  - contain null bytes
  - is not null (not only indicator of structure without value)
  - uses some well-defined but unspecified text encoding (i.e. not necessarily ASCII or UTF-8)
- remove binary functionality from keyNew keyVNew keyIsBinary keyIsString keyValue keyGetValueSize keyString keyGetString keySetString keyGetBinary keySetBinary
- keySetRaw -> keySetValue

## Rationale

## Implications

- metadata can also be binary
- binary types would be possible (e.g. nullable-string)

## Related Decisions

- [null](../6_implemented/null.md)

## Notes
