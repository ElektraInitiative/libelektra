- infos = Information about the spec plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/placements = postgetstorage presetstorage
- infos/status = maintained preview global nodep
- infos/description = copies meta data from spec namespace to other namespaces

## Introduction ##

The spec plugin is a global plugin that copies metadata from the `spec`-namespace to other namespaces using their key names as globbing expressions.
Globbing resembles regular expressions. They do not have the same expressive power, but are easier to use. The semantics are more suitable to match path names:

* _ matches any key name of just one hierarchy. This means it complies with any character except slash or null.
* ? satisfies single characters with the same exclusion.
* # matches Elektra array elements.
* Additionally, there are ranges and character classes. They can also be inverted.

The plugin copies the metadata of the corresponding `spec` key to every matching key in the other namespaces.

The spec plugin also provides basic validation and structural checking.
Specifically it supports:

* detection of invalid array key names
* detection of missing keys
* validation of array ranges
* validating the number of subkeys

## Configuration ##

### Actions ###

* `ERROR` yields an error when a conflict occurs
* `WARNING` adds a warning when a conflict occurs
* `INFO` adds a metakey `logs/spec/info` which can be used by logging plugins
* `IGNORE` ignores the conflict, this is the default value

### Conflicts ###

* Invalid array `member`: an invalid array key has been detected. e.g. `/#abc`
* Out of `range`: the array has more or less elements than specified by the `array` option.
* Invalid number of subkeys `count`: a key matching a `_` expression has more or less subkeys than specified by the `required` option.
* Conflicting metadata `collision`: the metakey that's supposed to be added already exists.  
* Missing keys `missing`: the key structure doesn't contain the required subkeys flagged with the `require` metakey in the `spec` namespace.
* Invalid keys `invalid`: keys that are subkeys of an invalid array member. e.g. `/#abc/key`

### Basic Configuration ###

`conflict/set` and `conflict/get` are used to specify what actions should be taken on conflicts. e.g. `conflict/get = ERROR` yields an error on every conflict.

### Per Conflict Configuration ### 

Actions can also be specified on a per conflict basis. Those actions take precedence over basic configuration options.

Examples: 

    conflict/get/member = WARNING
    conflict/set/range = ERROR

### Per Key Configuration ###

Actions can also be specified per-key. Those will take precedence over basic and per-conflict configurations.

Example:

    spec/test/#
        conflict/get/member = INFO

## Examples ##

Ini files can be found in [/examples/spec](/examples/spec) which should be PWD
so that the example works:

    cd ~e/examples/spec
    kdb global-mount        # mounts spec plugin by default
    kdb mount $PWD/spec.ini spec ni
    kdb mount $PWD/spectest.ini /testkey ni
    kdb export /testkey ni     # note: spec can only applied on cascading access

With spec mount one can use (in this case battery.ini needs to be installed in
`kdb file spec` (this should be preferred on non-development machines so that
everything still works after the source is removed):

    cp battery.ini $(dirname $(kdb file spec))
    kdb mount battery.ini spec/example/battery ni
    kdb spec-mount /example/battery
    kdb lsmeta /example/battery/level    # we see it has a check/enum
    kdb getmeta /example/battery/level check/enum    # now we know allowed values
    kdb set /example/battery/level low   # success, low is ok!
    kdb set /example/battery/level x     # fails, not one of the allowed values!

    cp openicc.ini $(dirname $(kdb file spec)) 
    kdb mount openicc.ini spec/freedesktop/openicc ni
    kdb spec-mount /freedesktop/openicc

    kdb ls /freedesktop/openicc # lets see the whole configuration
    kdb export spec/freedesktop/openicc ni   # give us details about the specification
    kdb lsmeta /freedesktop/openicc/device/camera/#0/EXIF_serial   # seems like there is a check/type
    kdb set "/freedesktop/openicc/device/camera/#0/EXIF_serial" 203     # success, is a long
    kdb set "set "/freedesktop/openicc/device/camera/#0/EXIF_serial" x   # fails, not a long

