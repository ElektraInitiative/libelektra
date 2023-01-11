- infos = Information about the spec plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>, Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = check apply
- infos/status = recommended productive nodep configurable hook experimental
- infos/description = allows to give specifications for keys

## Introduction

The spec plugin is a global plugin that copies metadata from the `spec`-namespace to other namespaces using their key names as globbing
expressions. Globbing resembles regular expressions. They do not have the same expressive power, but are easier to use. The semantics are
more suitable to match path names:

- `*` matches any key name of just one hierarchy. This means it complies with any character except slash or null.
- `?` satisfies single characters with the same exclusion.
- `#` matches Elektra array elements. (e.g. `#0`, `#_10`, `#__987`)
- `_` matches anything that `*` matches, except array elements.
- Additionally, there are ranges and character classes. They can also be inverted. For more infos take a look at the code documentation of
  `elektraKeyGlob()`.

The plugin copies the metadata of the corresponding `spec` key to every matching (see below) key in the other namespaces. The copied metadata
is removed again during `kdbSet` (if remained unchanged).

The spec plugin also provides basic validation and structural checking.
Specifically it supports:

- detection of invalid array key names
- detection of missing keys
- validation of array ranges (min/max array size)
- validating the number of subkeys of `_`

## Matching Algorithm

The matching of the spec (globbing) keys to the keys in the other namespaces is based on `elektraKeyGlob()`, which in turn is based on the
well known `fnmatch(3)`. However, there is special handling for array specifications (`#`) and wildcard specifications (`_`).

### Array Specifications

Keys which contain a part that is exactly `#` (e.g. `my/#/key` or `my/#`) are called array specifications. Instead of just matching the spec
key to any existing keys in other namespaces, theses keys are "instantiated". This means we lookup the array size (defined by the `array`
metakey) using a cascading `ksLookup`. This only looks at non-spec namespaces, if we don't find an array size their, we check the array
parent in the spec namespace. If we still have no array size, the array is deemed to be empty. For empty arrays, we will simply validate
that they are indeed empty.

For non-empty arrays "instantiation" takes place. This means that we replace each `#` part in the spec key by all the valid array elements
for this specification, up to the array size (which was already checked to be less than the allowed maximum). In other words, instead of
actually matching the globbing expression, we create a separate but identical specification for all valid array elements.

The purpose of "instantiation" is to allow specifying `default` values for array elements.

### Wildcard Specifications

Keys which contain a part that is exactly `_` (e.g. `my/_/key` or `my/_`) are called wildcard specifications. It would be nice to have
an "instantiation" procedure for `_` similar to the one for arrays. However, this is not possible with the current implementation, since
there is no way of knowing in advance, which keys matching the globbing expression may be requested via `ksLookup`.

Instead `_` is simply treated like `*` during matching. Afterwards we check that no array elements were matched.

## Specification and Validation

The basic functionality of the plugin is to just copy (using `keyCopyMeta()` so we don't waste memory) the metadata of spec keys to all
matching (as described above) keys in other namespaces. This ensures that other plugins can do their work as expected, without manually
setting metadata on every key. If any metakey we want to copy already exists on the target key (with a different value), this causes a
`collision` conflict.

In addition to the basic functionality, the plugin does some validation itself.

### Default Values

If a spec key has the metakey `default` set and the key does not exist in other namespaces, we create a special (cascading) key that will
be found by `ksLookup`. This key has the `default` value as its value. We also copy over metadata as always.

A similar procedure takes place for `assign/condition`, but in this case, we don't set the value of the key. We only create it and copy
metadata.

### Required Keys

If a spec key has the metakey `require` (the value of this metakey is irrelevant and ignored), we ensure that this key exists in at least
one other namespace, i.e. it can be found using a cascading `ksLookup`. If the key cannot be found, this causes a `missing` conflict.

### Array Size Validation

As hinted to above, we validate array sizes. If a spec key `x/#` is given, and the spec key `x` has the metakey `array/min` or `array/max`
set, we validate the array size (given as metakey `array` on `x`) is within the limits of `array/min` and `array/max`. Both `array/min` and
`array/max` have to be valid array-elements similar to `array`. If the array size is out of bounds, this causes a `range` conflict.

We also check that the array only contains actual array elements. If this not the case, this causes a `member` conflict.

Note: We don't actually validate that the array doesn't contain elements above the given array size. This is because it doesn't have anything
to do with the specification, whether the array contains additional elements. Note also that we only copy metadata onto elements within
the bounds of the array size.

## Configuration

There are various ways in which a conflict can occur during the validation process. To handle these conflicts, we provide various
configuration options.

### Conflicts

The possible conflicts are:

- Invalid array `member`: an invalid array key has been detected. e.g. `/#abc`, or `/x` (i.e. non-array-element) if array (`#`) was specified
- Out of `range`: the array has more or less elements than specified by the `array/min` and `array/max` metakeys.
- Missing keys `missing`: the key marked as `require`d wasn't found.
- Invalid keys `invalid`: the specification or a key was invalid
- Conflicting metadata `collision`: the metakey that's supposed to be added already exists.

#### Configuration Keys

To define what actions should be taken on on conflicts during `kdbGet` and `kdbSet` respectively use the keys `conflict/set` and
`conflict/get` in the plugin configuration.

This base configuration can be overridden on a per-conflict basis to provide more granularity. The keys for this are (replace `_` with
`get` or `set` accordingly):

```
conflict/_/member
conflict/_/range
conflict/_/missing
conflict/_/invalid
conflict/_/collision
```

For even more granularity, the above per-conflict metakeys can also be specified on individual keys.

The allowed values for the conflict keys are always the same:

- `ERROR` yields an error when a conflict occurs
- `WARNING` adds a warning when a conflict occurs
- `INFO` adds a metakey `logs/spec/info` to the parent-key which can be used by logging plugins.
  `logs/spec/info` is an array, each element is one conflict.
- any other value ignores the conflict, this includes if the conflict key is not given (i.e. the default)

There is also the special key `missing/log`. If it is set to `1`, the plugin will create the meta array `logs/spec/missing/#`.
This array will contain a list of the missing keys. The key `missing/log` can only be part of the main plugin configuration,
not individual keys' metakeys. It also applies to `kdbGet` and `kdbSet` calls.

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

## Known Issues

- In MINGW32 builds, there is no globbing and therefore no support for key names with # and \_.
- Added metadata is not correctly removed during `kdbSet`, if the corresponding spec key was modified.
- Default values do not work if globbing is involved.
- By default, keys tagged with `require` do not emit errors even if not present
  (https://issues.libelektra.org/1024)
