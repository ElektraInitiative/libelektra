- infos = Information about the spec plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/placements = postgetstorage presetstorage
- infos/status = maintained preview global
- infos/description =

## INTRODUCTION ##

The spec plugin is a global plugin that copies metadata from the `spec`-namespace to other namespaces using their key names as globbing expressions.
Globbing resembles regular expressions. They do not have the same expressive power, but are easier to use. The semantics are more suitable to match path names:

* _ matches any key name of just one hierarchy. This means it complies with any character except slash or null.
* ? satisfies single characters with the same exclusion.
* # matches elektra array elements.
* Additionally, there are ranges and character classes. They can also be inverted.

The plugin copies the metadata of the corresponding `spec` key to every matching key in the other namespaces.

The spec plugin also provides basic validation and struct checking.

It supports:

* detection of invalid array key names
* detection of missing keys
* validation of array ranges
* validating the number of subkeys

## CONFIGURATION ##

### ACTIONS ###

* `ERROR` yields an error when a conflict occurres
* `WARNING` adds a warning when a conflict occurres
* `INFO` adds a metakey `logs/spec/info` which can be used by logging plugins
* `IGNORE` ignores the conflict, this is the default value

### CONFLICTS ###

* Invalid array `member`: an invalid array key has been detected. e.g. `/#abc`
* Out of `range`: the array has more or less elements than specified by the `array` option.
* Invalid number of subkeys `count`: a key matching a `_` expression has more or less subkeys than specified by the `required` option.
* Conflicting metadata `collision`: the metakey that's supposed to be added already exists.  
* Missing keys `missing`: the key structure doesn't contain the required subkeys flagged with the `require` metakey in the `spec` namespace.
* Invalid keys `invalid`: keys that are subkeys of an invalid array member. e.g. `/#abc/key`

### BASIC CONFIGURATION ###

`conflict/set` and `conflict/get` are used to specify what actions should be taken on conflicts. e.g. `conflict/get = ERROR` yields an error on every conflict.

### PER CONFLICT CONFIGURATION ### 

Actions can also be specified on a per conflict basis. Those actions take precedence over basic configuration options.

Examples: 
```
conflict/get/member = WARNING
conflict/set/range = ERROR
```

### PER KEY CONFIGURATION ###

Actions can also be specified per-key. Those will take precedence over basic and per-conflict configurations.

Example:
```
spec/test/#
    conflict/get/member = INFO
```

## EXAMPLES ##

see `src/plugins/spec/test`

