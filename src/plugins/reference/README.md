- infos = Information about the reference plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained unittest libc nodep writeonly
- infos/metadata = check/reference check/reference/restrict check/reference/restrict/#
- infos/description = Plugin for validating singular or recursive references

## Introduction

This plugin serves one single purpose: the validation of references from key to another inside the KDB.

To specify a key as a reference add the metakey `check/reference`. Currently the metakey
supports the three values `single`, `recursive` and `alternative`. While the value
`alternative` only makes sense in a certain context inside the `recursive` mode, the other
two values define to different modes of operation for the plugin.

If a key has the metakey `check/reference` with value `single`, the plugin reads the value
of the key and produces an error, if the value is not a valid (i.e. resolvable) reference.

If a key has metakey `check/reference` with value `recursive`, the plugin constructs a
reference graph starting with the marked key. (for the exact construction see below)
In this reference graph each node corresponds to a key in the KDB, while each edge
represents a reference between to keys. The plugin will produce an error, if this graph
is not a directed acyclic graph or contains any invalid references.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

### Resolution of References

The plugin will try to resolve all keys marked with the metakey `check/reference` as references.
The only exception to this rule is, if the value of such a key is a valid array name (i.e. `#0`,
`#_10`, ...). In that case, the plugin will try to resolve the values of the array elements,
directly below the marked key. And treats the marked key as referencing multiple other keys.

The resolution of the references into key names goes as follows:

- If the reference value starts with `./` or `../`, it will be interpreted as relative to
  the current key or the parent of the current key.
- If the reference value starts with `@/`, it will be relative to the parent-key used in the
  call to `kdbGet`.
- All other values are treated as absolute and interpreted as a keyname directly.
- `.` and `..` will be treated the same way as in Unix paths. However, the plugin will emit
  warnings, if `.` is used anywhere other than before the first slash. Equally using `..`
  after a path segment other than `..` itself, will result in a warning. This is because these
  use cases are redundant and might be (or result in) mistakes. Here are a few examples of what
  results in warnings, and what doesn't:
  - `./system:/key` no warning
  - `system:/key` no warning
  - `system:/./key` warning, redundant use of `.`
  - `../../../key` no warning
  - `../key/../otherkey`warning, redundant use of `.`

### Construction of the Reference Graph

The process starts with a key `X`, which has the metakey `check/reference` set to the value
`recursive`. This key `X` must be an array key. The basename of `X` will be called the `refname`.
For each element in the array run the following process:

1. Interpret the current key `K`s value as a reference and throw an error, if the reference
   is not valid.
2. If there is no node for `K` in the reference graph create one. If there is no node for
   `R` in the reference graph create one. Insert an edge from `K` to `R` into the graph.
3. Find a key `K1` directly below `R` that has the `refname` as its basename. Interpret this
   key as an array if found. For each array element, start the process recursively from 1
   and then continue with 4.
4. Find any key `K2` directly below `R` that has the metakey `check/reference` set to
   `alternative`. Interpret any such key as an array. For each array element, start the
   process recursively from 1, but this time using the basename of `K2` (the of the array)
   as the `refname`.

Once the whole process is finished, we will have a graph of the whole reference structure
stemming from `X`. This graph can then be check for acyclicity.

### Restriction of References

Without any additional intervention, the plugin accepts the name of any existing key as valid
reference value. Existing in this context means, the key either has a value or metadata
associated with it. In recursive mode a key is also said to exist, if a key with the current
`refname` as a basename exists (has value or metadata) directly below the key in question.
If, however, the key containing the reference value has the `check/reference/restrict` metakey
set (even if the value is empty), its value has to be taken into account.

If the value `check/reference/restrict` is anything but a valid array-element-name, i.e. a `#`
followed by `n` underscores (`_`) and `n+1` digits (`0-9`), its value will be used directly.
Otherwise `check/reference/restrict` has to be a valid array and its elements will be treated
as alternative restriction. Only one of these restriction has to be fulfilled for a reference to
be valid.

Each restriction is first resolved as if it was a reference itself (see
[Resolution of references](#resolution-of-references)). The resolved value is then used as an
Elektra-style globbing pattern. For the supported see [elektra-globbing](/src/libs/globbing).

If a keyname matches the globbing expression it will be considered a valid reference
(as long as the key actually exists).

Note: If the restriction is the empty string `""` **no** keyname will match, meaning
the reference key annotated with this metadata has to be a leaf node in the reference graph,
and therefore cannot have a value (other than the empty string `""`).

## Usage

```sh
# Mount the plugin
sudo kdb mount referencetest.dump user:/tests/reference dump reference

# Mark a key as a single reference
kdb meta set user:/tests/reference/singleref check/reference single

# Try setting an invalid reference
kdb set user:/tests/reference/singleref user:/tests/reference/referred1
# RET: 5
# STDERR: .*Reference 'user:/tests/reference/referred1', set in key 'user:/tests/reference/singleref', does not reference an existing key.*

# Create referred key ...
kdb set user:/tests/reference/referred1 ""
#> Create a new key user:/tests/reference/referred1 with string ""

# ... and try again
kdb set user:/tests/reference/singleref user:/tests/reference/referred1
#> Set string to "user:/tests/reference/referred1"

# Cleanup
kdb rm user:/tests/reference/singleref
kdb rm user:/tests/reference/referred1

# Unmount the plugin
sudo kdb umount user:/tests/reference
```

## Examples

The [examples](examples/) directory contains a few examples:

- [alternative](examples/alternative/) shows how the `alternative` value of `check/reference` gets processed.
- [complex](examples/complex/) shows how the plugin can be used together with the spec plugin, to
  validate complex recursive structures.
