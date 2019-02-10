- infos = Information about the yawn plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = directoryvalue yamlsmith
- infos/provides = storage/yaml
- infos/recommends =
- infos/placements = getstorage
- infos/status = maintained unittest preview experimental unfinished nodoc concept discouraged
- infos/metadata =
- infos/description = This storage plugin uses an Earley parser to read YAML files

# YAwn

## Introduction

This plugin uses [YAEP](https://github.com/vnmakarov/yaep) to parse [YAML](http://yaml.org) data.

## Dependencies

This plugin requires [YAEP](https://github.com/vnmakarov/yaep#installing).

## Examples

### Mappings

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yawn yawn

# Set and retrieve some basic values
kdb set user/tests/yawn 'is obviously a move introduced in Generation III of Pokemon' # Source: Bulbapedia
kdb set user/tests/yawn/ing 'is something similar'

kdb get user/tests/yawn/
#> is obviously a move introduced in Generation III of Pokemon
kdb get user/tests/yawn/ing
#> is something similar

kdb ls user/tests/yawn
#> user/tests/yawn
#> user/tests/yawn/ing

# Undo modifications
kdb rm -r user/tests/yawn
sudo kdb umount user/tests/yawn
```

### Arrays

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yawn yawn

kdb set user/tests/yawn/movies
kdb set user/tests/yawn/movies/#0 'YAwn I: Time to Wake Up'
kdb set user/tests/yawn/movies/#1 'YAwn II: The Awakening'
kdb set user/tests/yawn/movies/#2 'YAwn III: The Sleepover'
kdb set user/tests/yawn/movies/#3 'YAwn IV: YAwn Again'

# Retrieve last array index
kdb getmeta user/tests/yawn/movies array
#> #3

kdb get user/tests/yawn/movies/#1
#> YAwn II: The Awakening

# Undo modifications
kdb rm -r user/tests/yawn
sudo kdb umount user/tests/yawn
```

### Error Messages

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yawn yawn

# Manually add some data
printf -- ' - Brutus\n' >  `kdb file user/tests/yawn`
# Add element with incorrect indentation
printf -- '- Burst'     >> `kdb file user/tests/yawn`

# Try to retrieve data
kdb ls user/tests/yawn
# RET: 5
# STDERR-REGEX: Reason: .*/config.yaml:2:1: Syntax error on token number 5: “<Token, SEQUENCE_START, SEQUENCE START, 2:1–2:1>”

# Fix syntax error
printf -- ' - Brutus\n' >  `kdb file user/tests/yawn`
printf -- ' - Burst'    >> `kdb file user/tests/yawn`
kdb ls user/tests/yawn
#> user/tests/yawn
#> user/tests/yawn/#0
#> user/tests/yawn/#1

# Undo modifications
kdb rm -r user/tests/yawn
sudo kdb umount user/tests/yawn
```

## Limitations

The plugin has the same limitations as [YAMBi ](../yambi/).
