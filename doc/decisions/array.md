# Arrays

## Issue

Currently it is inefficient to detect the length of an array and
it is impossible to know if an key (without subkeys) should be
an array or not.

For the latter problem different workarounds exist, such as
`###empty_array` in `yajl`.

## Constraints

- None


## Assumptions

- None


## Considered Alternatives

- `###empty_array` as in `yajl`, problem: does not allow efficient access of first element
- store length (and not last element), problem: needs prepending of `#_...`
- store element after last element (C++-Style), problem: very unusual style
- use value and not metadata array, problem: is ambiguous


## Decision

Store length in metadata `array` of key, or keep empty if empty array.
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


## Argument

- Is very similar to `binary` metadata.
- The key alone suffices to know if its an array
- one can distuinigish an array with keys that are called by chance #0

## Implications

- yajl needs to be fixed
- metadata library needs to be adapted
- spec plugin needs to be fixed

## Related decisions

- [Global Validation](global_validation.md)


## Notes

https://github.com/ElektraInitiative/libelektra/issues/182
