# Holes and Non-leaf values in KeySets

A hole is the absence of a key, which has keys below it, e.g. if `some/key` is missing in a property file:

```ini
some = value
some/key/below = value
```

`some` has a non-leaf value. Another example of a non-leaf value in XML (`abc`):
`<abc>value<def>value2</def></abc>` interpreted by xerces plugin:

```
abc/def = value2
abc = value
```

## Problem

Config files ideally do not copy any structure if they only want to
set a single key.

## Constraints

- strongly hierarchically structured data must still be supported

## Assumptions

## Considered Alternatives

- data structure must always be complete
- prohibit non-leaves values

## Decision

Support holes and values for non-leaves in a KeySet if the underlying format allows it.

If the underlying format does not support it and there is also not an obvious
way how to circumvent it -- e.g., JSON which does not have comments -- holes and
values in non-leaves can be supported with key names starting with ®elektra.

## Rationale

- It fits very good to the idea of key-value.
- Some formats support it (e.g. XML supports non-leaves values; property-files support holes).
- It can be useful for migration purposes, e.g. there is `/some/key`, and later
  `/some/key/enable` gets added. Then it is beneficial if `/some/key` still can
  hold a value.

## Implications

## Related Decisions

## Notes
