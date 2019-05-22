- infos = Information about the boolean plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = reviewed conformant compatible coverage specific unittest tested nodep configurable obsolete
- infos/metadata =
- infos/description = canonicalizes boolean values

**This plugin is obsolete:** Please use the `type` plugin instead.

## Introduction

Different configurations might use different values for `TRUE`/`FALSE`. The `boolean` plugin canonicalizes boolean values.

## Example

```sh
# Mount plugin
sudo kdb mount config.ecf user/tests/boolean dump boolean

# By default the plugin uses `1` (true) and `0` (false) to represent boolean values
kdb set user/tests/boolean/truthiness false
kdb setmeta user/tests/boolean/truthiness type boolean
kdb get user/tests/boolean/truthiness
#> 0

# The plugin does not change ordinary values
kdb set user/tests/boolean/key value
kdb get user/tests/boolean/key
#> value

# Undo changes
kdb rm -r user/tests/boolean
sudo kdb umount user/tests/boolean
```

## Configuration

- `on/true` specifies the normalized value for `TRUE`. Default: `1`
- `on/false` specifies the normalized value for `FALSE`. Default: `0`
- `on/invalid` specifies the normalized value for keys that don't contain a valid boolean value. Values: `TRUE`, `FALSE`. Default: `TRUE`.
- `on/invalid/warning` specifies if the plugin will yield a warning when an invalid value is found during kdbGet. Values: `TRUE`, `FALSE`. Default: `TRUE`
- `true` specifies a (case insensitive) list of valid `TRUE`-values. The values have to be separated by a `;`. Default: `TRUE; 1; ON; ENABLE; ENABLED; YES`
- `false` specifies a (case insensitive) list of valid `FALSE`-values. The values have to be separated by a `;`. Default: `FALSE; 0; OFF; DISABLE; DISABLED; NO; NOT`
