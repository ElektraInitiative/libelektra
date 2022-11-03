# Null

## Problem

Null keys do not have a semantic and make API more difficult.

## Constraints

## Assumptions

## Considered Alternatives

- remove null keys, i.e. keyValue always returns a string
  (e.g. empty string on new keys)

## Decision

Null keys get clear semantics: they are for representing keys that do not have a value, i.e.,
only represent structure in configuration files.

For example, an INI `section` is a null key:

```ini
[section]
key = value
```

For example, in other config formats there might be keys without any value. Here
`key1` would be a null-key and thus is different to the value of `key2`, i.e.,
an empty string:

```ini
key1
key2 = ""
```

## Rationale

## Implications

## Related Decisions

- [Key Name](../0b_rejected/separate_key_name.md)

## Notes
