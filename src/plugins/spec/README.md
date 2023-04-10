- infos = Information about the spec plugin is in keys below
- infos/author = Tomislav Makar <tmakar23@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = check apply
- infos/placements = hook
- infos/status = recommended productive nodep configurable global
- infos/description = allows to give specifications for keys

## Introduction

The spec plugin is a global plugin that copies metadata from the `spec`-namespace to other namespaces using their key names as globbing
expressions. Globbing resembles regular expressions. They do not have the same expressive power, but are easier to use. The semantics are
more suitable to match path names:

- `*` matches any key name of just one hierarchy. This means it complies with any character except slash or null.
- `#` matches Elektra array elements. (e.g. `#0`, `#_10`, `#__987`).
- `_` matches anything that `*` matches, except array elements.

The plugin copies the metadata of the corresponding `spec` key to every matching (see below) key in the other namespaces.
The copied metadata is removed again during `kdbSet` (if remained unchanged).

The spec plugin also provides basic validation and structural checking.
Specifically it supports:

- detection of invalid array key names
- detection of missing keys
- validation of array ranges (min/max array size)
- validating the number of subkeys of `_`

## Matching Algorithm

The matching of the spec (globbing) keys to the keys in the other namespaces is based on `elektraKeyGlob()`, which in turn is based on the
well known `fnmatch(3)`. However, there is special handling for array specifications (`#`) and wildcard specifications (`_`).

### Default Values

If a spec key has the metakey `default` set and the key does not exist in other namespaces, we create a key in the `default:/` namespace.
This key has the `default` value as its value. We also copy over metadata as always.

### Array Specifications

Keys which contain a part that is exactly `#` (e.g. `my/#/key` or `my/#`) are called array specifications.
These keys are instantiated in order to support `default` values.
If the key does not exist and `default` is specified in the spec namespace, the key is created in the `default` namespace.
We also lookup the array size (defined by the `array` metakey) using a cascading `ksLookup`.
This only looks at non-spec namespaces, if we don't find an array size there, we check the array parent in the spec namespace.
If we still have no array size, the array is deemed to be empty. For empty arrays, we will simply validate that they are indeed empty.

### Wildcard Specifications

Keys which contain a part that is exactly `_` (e.g. `my/_/key` or `my/_`) are called wildcard specifications.
It would be nice to have an "instantiation" procedure for `_` similar to the one for arrays.
However, this is not possible with the current implementation, since there is no way of knowing in advance, which keys matching the globbing
expression may be requested via `ksLookup`.

Instead `_` is simply treated like `*` during matching. Afterwards we check that no array elements were matched.

## Specification and Validation

The basic functionality of the plugin is to just copy (using `keyCopyMeta()` so we don't waste memory) the metadata of spec keys to all
matching (as described above) keys in other namespaces.
This ensures that other plugins can do their work as expected, without manually setting metadata on every key.
If a metakey on a target key already exists with different value, it gets overridden.

In addition to the basic functionality, the plugin does some validation itself.

### Required Keys

If a spec key has the metakey `require` (the value of this metakey is irrelevant and ignored), we ensure that this key exists in at least
one other namespace, i.e. it can be found using a cascading `ksLookup`. If the key cannot be found, this causes an `error`.

### Array Size Validation

As hinted to above, we validate array sizes. If a spec key `x/#` is given, and the spec key `x` has the metakey `array/min` or `array/max`
set, we validate the array size (given as metakey `array` on `x`) is within the limits of `array/min` and `array/max`.
Both `array/min` and `array/max` have to be valid array-elements similar to `array`.
If the array size is out of bounds, this causes an `error` for `kdbSet` or a warning for `kdbGet`.

Note: We don't actually validate that the array doesn't contain elements above the given array size.
This is because it doesn't have anything to do with the specification, whether the array contains additional elements.
Note also that we only copy metadata onto elements within the bounds of the array size.

### Array Specification and Wildcard Specification (Collision)

A collision is when two specification keys exist, one as wildcard specification, the other as array specification, and it is not clear in
this case what the correct specification is.

```text
spec:/server/_/name => meta:/description = value1

spec:/server/#/name => meta:/description = value2
```

The spec plugin does not know what specification to take in this case, so it appends an `error` or `warning` (if kdbGet).

## Error Handling

In case there is an error, warning or information, it is appended to the `parent key`.

### Example

```text
parentKey = "system:/sw/org"

# in case there is an error
system:/sw/org/error/...

# in case there is a warning
system:/sw/org/warning/...

# in case there is an info
system:/sw/org/info/description
```

If there is an error, the `spec` plugin returns `ELEKTRA_PLUGIN_STATUS_ERROR`, otherwise `ELEKTRA_PLUGIN_STATUS_SUCCESS`.

### Cases

- Key is required but does not exist
  - In this case an `error` is appended to the `parent key`
- Key is in specification but does not exist in any other namespace
  - Just append an info to the `parent key`
- Key has default but does not exist
  - In this case the key is created

## Examples

<!-- FIXME [new_backend]: outdated -->

Ni files can be found in [/examples/spec](/examples/spec) which should be PWD
so that the example works:

```sh
cd ../../../examples/spec
#sudo kdb global-mount        # spec plugin should be mounted by default
sudo kdb mount $PWD/spec.ini spec:/ ni
sudo kdb mount $PWD/spectest.ini /testkey ni
kdb export /testkey ni     # note: spec can only applied on cascading access
```

With spec mount one can use (in this case battery.ini needs to be installed in
`kdb file spec:/` (this should be preferred on non-development machines so that
everything still works after the source is removed):

```sh
sudo cp battery.ini $(dirname $(kdb file spec:/))/
sudo kdb mount battery.ini spec:/example/battery ni
sudo kdb spec-mount /example/battery
kdb meta-ls /example/battery/level    # we see it has a check/enum
kdb meta-get /example/battery/level check/enum    # now we know allowed values
kdb set /example/battery/level low   # success, low is ok!
kdb set /example/battery/level x     # fails, not one of the allowed values!
```

```sh
cp openicc.ini $(dirname $(kdb file spec:/))/
sudo kdb mount openicc.ini spec:/freedesktop/openicc ni
sudo kdb spec-mount /freedesktop/openicc

kdb meta-set /freedesktop/openicc/device/camera/ array "#1"
kdb ls /freedesktop/openicc # lets see the whole configuration
kdb export spec:/freedesktop/openicc ni   # give us details about the specification
kdb meta-ls spec:/freedesktop/openicc/device/camera/#0/EXIF_serial   # seems like there is a check/type
kdb set "/freedesktop/openicc/device/camera/#0/EXIF_serial" 203     # success, is a long
kdb set "/freedesktop/openicc/device/camera/#0/EXIF_serial" x   # fails, not a long
```

### Known limitations

- `#` and `_` keys do not work on MINGW
- No defaults for `_` globbing character
