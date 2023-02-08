# Semantics in Key Names

## Problem

It can get quite cumbersome to find out about key interrelations like arrays.

## Constraints

## Assumptions

## Considered Alternatives

The alternative would be to have semantics in key names, with following advantages:

- maybe less metadata to save memory (only array)

## Decision

Do not encode any semantics into the key names.
All semantics must be in metadata.

Nevertheless, there are guidelines (without any checks in `keySetBaseName`):

- `#` is used to indicate that array numbers follow.
- `®` is used to indicate that some information was encoded in the key name.
  This is usually only needed internally in storage plugins.
- The UTF-8 sequence `®elektra` (i.e. the 9-byte sequence `C2 AE 65 6C 65 6B 74 72 61`) is reserved, see key name documentation.

There are, however, rules and conventions which syntax to use for specific semantics.
The `spec` plugin guards these rules.

## Rationale

- for consistency, whenever possible, metadata should be preferred
- no escaping of key base names necessary
- it is very unlikely that `®elektra` collides with a real key base name
  a user wanted to have
- `®elektra` makes very clear that there is a special reserved meaning
- `®elektra` UTF-8 encoding decodes to "some character" + ® in many 8-bit encodings
  (including ISO 8859-1 aka Latin1 and Windows (Codepage) 1252,
  in the encoding `C`, however, you get `''$'\302\256''elektra'`)

## Implications

## Related Decisions

- [Arrays](../5_partially_implemented/array.md)
- [Base Names](../6_implemented/base_name.md)

## Notes
