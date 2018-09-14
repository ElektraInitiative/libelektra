- infos = Information about the template plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage
- infos/status = nodep libc preview experimental unfinished
- infos/metadata = check/reference check/reference/restrict
- infos/description = Useful plugin for validating singular or recursive references

## Introduction

This plugin serves one single purpose: the validation of reference values inside of keys.

To specify a key as a reference add the metakey `check/reference`. Currently the metakey 
supports the three values `single`, `recursive` and `alternative`. While the value 
`alternative` only makes sense in a certain context inside the `recursive` mode, the other
two values define to different modes of operation for the plugin. 

If a key has the metakey `check/reference` with value `single`, the plugin reads the value
of the key and produces an error, if the value is not a valid reference.

If a key has metakey `check/reference` with value `recursive`, the plugin constructs a 
reference graph starting with the marked key. (for the exact construction see below) 
In this reference graph each node corresponds to a key in the KDB, while each edge 
represents a reference between to keys. The plugin will produce an error, if this graph
is not a directed acyclic graph or contains any invalid references.

Additionally to this basic functionality the plugin allows one to restrict the set of valid
references using the `check/reference/restrict` metakey. The exact mechanism for this is 
described below.

### Resolution of references
If the key `A` is interpreted as a reference by the plugin, it has to be an array. Each of 
the array elements must contain a so called reference value. This reference value will be 
resolved into a keyname, which will then be validated. The resolution goes as follows:

* If the reference value starts with `/`, it is treated as absolute and interpreted as a 
  keyname directly.
* Otherwise the reference value will be interpreted as relative to the **parent** of `A`.
  That means a reference which does not contain a `/` will also was refer to a sibling of `A`,
  the reference array key.
* `.` and `..` will be treated the same way as in UNIX paths. If these are used in an absolute
  reference, the plugin will emit a warning, because their use in this case is unnecessary and 
  might be a mistake. The same is true, if `.` is used anywhere, but at the start of the 
  reference.

### Construction of the reference graph

The process starts with a key `X`, which has the metakey `check/reference` set to the value
`recursive`. This key `X` must be an array key. The basename of `X` will be called the `refname`.
For each element in the array run the following process:

1. Interpret the current key `K`s value as a referemce and throw an error, if the reference
   is not valid.
2. If there is no node for `K` in the reference graph create one. If there is no node for 
   `R` in the reference graph create one. Insert an edge from `K` to `R` into the graph.
3. Find a key `K1` directly below `R` that has the `refname` as its basename. Interpret this 
   key as an array, if found. For each array element, start the process recursively from 1 
   and then continue with 4.
4. Find any key `K2` directly below `R` that has the metakey `check/reference` set to 
   `alternative`. Interpret any such key as an array. For each array element, start the 
   process recursively from 1, but this time using the basename of `K2` (the of the array)
   as the `refname`.

Once the whole process is finished, we will have a graph of the whole reference structure 
stemming from `X`. This graph can then be check for acyclicity.

### Restriction of references

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

The restrictions themselves are UNIX-like pathname globbing expression, as defined by 
[`fnmatch(3)`](http://man7.org/linux/man-pages/man3/fnmatch.3.html) using the `FNM_PATHNAME` 
option. Path segments equal to `.` and `..` are interpreted like they are in UNIX-paths. If 
the restriction starts with a `/` it will be treated as an absolute path, and the use of `.` 
and `..` is unnecessary and discouraged. Therefore a warning will be emitted, if `.` or `..` 
are encountered. The same is true if a path segment other then the first one is `.`.

If a keyname matches the globbing expression it will be considered a valid reference 
(as long as the key actually exists). Additionally if the globbing expression ends in `...` every key
below a matching key also matches. In other words: The globbing expression must only match a prefix
of the actual keyname.

Lastly if the restriction is the empty string `""` **no** keyname will match, meaning
the reference key annotated with this metadata has to be a leaf node in the reference graph,
and therefore cannot have a value (other than the empty string `""`).

## Usage

```sh
# Mount the plugin
sudo kdb mount referencetest.dump user/tests/reference dump reference

# Mark a key as a single reference
kdb setmeta user/tests/reference/singleref check/reference single

# Try setting an invalid reference
kdb set user/tests/reference/singleref user/tests/reference/referred1

# Create referred key ...
kdb set user/tests/reference/referred1 ""

# ... and try again
kdb set user/tests/reference/singleref user/tests/reference/referred1
```

## Examples

The [examples](examples/) directory contains a few examples:

* [simple-rec](examples/simple-rec/) is a simple recursive example.
* [restrict](examples/restrict/) is an example for using reference restrictions.
* [alternative](examples/alternative/) shows how the `alternative` value of `check/reference` gets processed.
* [complex](examples/complex/) shows how the plugin can be used together with the spec plugin, to 
  validate complex recursive structures.