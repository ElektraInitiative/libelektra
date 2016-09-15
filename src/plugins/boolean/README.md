- infos = Information about the boolean plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = recommended productive maintained reviewed conformant compatible coverage specific unittest tested nodep configurable
- infos/metadata =
- infos/description =

## Introduction ##

Different configurations might use different values for `TRUE`/`FALSE`. The `boolean` plugin canonicalizes boolean values.

## Configuration ##

- `on/true` specifies the normalized value for `TRUE`. Default: `1`
- `on/false` specifies the normalized value for `FALSE`. Default: `0`
- `on/invalid` specifies the normalized value for keys that don't containe a valid boolean value. Values: `TRUE`, `FALSE`. Default: `TRUE`.
- `on/invalid/warning` specifies if the plugin will yield a warning when a invalid value is found during kdbGet. Values: `TRUE`, `FALSE`. `Default: `TRUE`
- `true` specifies a (case insensitive) list of valid `TRUE`-values. The values have to be separated by a `;`. Default: `TRUE; 1; ON; ENABLE; ENABLED; YES` 
- `false` specifies a (case insensitive) list of valid `FALSE`-values. The values have to be separated by a `;`. Default: `FALSE; 0; OFF; DISABLE; DISABLED; NO; NOT`
