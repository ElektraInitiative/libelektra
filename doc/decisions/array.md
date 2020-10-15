# Arrays

## Problem

Currently it is inefficient to detect the length of an array and
it is impossible to know if a key (without subkeys) should be
an array or not.

## Constraints

- None

## Assumptions

- Meta-data arrays simply work by convention as they are not serialized in special ways nor they get validated.
  The convention will be the same but without any metadata marker.

## Considered Alternatives

- `###empty_array` as in `yajl`, problem: does not allow efficient access of first element
- store length (and not last element), problem: needs prepending of `#_...`
- store element after last element (C++-Style), would not fit nicely with key/value
- use value and not the metadata `array`, problem: is ambiguous

## Decision

Store length in metadata `array` of key, or keep metadata `array` empty if empty array.
For example (`ni syntax`, sections used for metadata):

```
myarray/#0 = value0
myarray/#1 = value1
myarray/#2 = value2
myarray/#3 = value3
myarray/#4 = value4
myarray/#5 = value5
[myarray]
  array = #5
```

## Rationale

- Is very similar to `binary` metadata.
- The key alone suffices to know if its an array
- One can distinguish an array with keys that are called by chance e.g. `#0`

## Implications

- yajl needs to be fixed
- metadata library needs to be adapted
- spec plugin needs to be fixed

## Related Decisions

- [Global Validation](global_validation.md)

## Notes

https://github.com/ElektraInitiative/libelektra/issues/182
