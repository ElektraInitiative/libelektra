# Arrays

## Problem

Currently it is inefficient to detect the length of an array and
it is impossible to know if a key (without subkeys) should be
an array or not.

## Constraints

- None

## Assumptions

## Considered Alternatives

- Metadata arrays simply work by convention as they are not serialized in special ways nor they get validated.
- `###empty_array` as in `yajl`, problem: does not allow efficient access of first element
- store length (and not last element), problem: needs prepending of `#_...`
- store element after last element (C++-Style), would not fit nicely with key-value
- use value and not the metadata `array`, problem: is ambiguous
- use metadata on all children

## Decision

Store length in metadata `array` of key, or keep metadata `array` empty if empty array.
Only children that have `#` syntax are allowed in a valid array.
The index start with `#0`.
Both `keyAddName("#12")` or `keyAddBaseName("#_12")` is allowed to add the 13th index.

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

It is not allowed to have anything else than `#5` in the metadata `array`,
e.g. even `#4` would be a malformed array.

With the metadata `array` in `spec:/` the `spec` plugin will add the
`array` marker with the correct length.
This needs to be happening early, so that plugins can rely on having
correct arrays.

For example:

```
spec:/myarray      # <- has array marker

user:/myarray      # <- either has correct array marker or no array marker
user:/myarray/#0
user:/myarray/#1
```

Here, the `spec` plugin would add `array=#1` to `user:/myarray` if it was not there before.

To look up an array, first do `ksLookupByName (ks, "/myarray", 0)` on the parent.
With the last index you get from its metadata `array`, iterate over the children.
A cascading lookup on every individual child is also needed to make sure that overrides on individual
elements are respected.

For example:

```
spec:/myarray    # <- contains the specification for the array
spec:/myarray/#  # <- contains the specification for the array elements

dir:/myarray/#0  # <- not an array, just an override for user:/myarray/#

user:/myarray    # <- with metadata array=#0, this would be the array we get
user:/myarray/#0

system:/myarray  # <- not found in cascading lookup, as user:/myarray exists
```

The `spec` plugin should check if it is a valid array, i.e.:

- that the parent key always contains the metadata `array`,
- that the correct length is in `array`,
- that the array only contains `#` children, and
- that the children are numbered from `#0` to `#n`, without holes.

## Rationale

- Is very similar to `binary` metadata.
- The key alone suffices to know if it's an array
- One can distinguish an array with keys that are called by chance e.g. `#0`

## Implications

- yajl needs to be fixed
- metadata library needs to be adapted
- spec plugin needs to be fixed, a lot of work needed there.
  Most amount of work is to detect misstructured nested arrays (`#` intermixed with non `#` keys)
  which is a possibility also in all the alternatives of this decision.
- A `user:/` or `dir:/` key can change the semantics of a `system:/` array,
  if not avoided by `spec`.
- user-facing documentation should contain a note like:
  "Mixing array and non-array keys in arrays is not supported.
  In many cases the trivial solution is to move the array part into a separate child-key."

## Related Decisions

- [Hooks](hooks.md)
- [Global Validation](global_validation.md)
- [Base Names](base_name.md)
- [Metadata in Spec Namespace](spec_metadata.md)
- [Spec Expressiveness](spec_expressiveness.md)

## Notes

https://github.com/ElektraInitiative/libelektra/issues/182
