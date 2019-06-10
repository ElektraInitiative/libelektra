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

### Boolean Values

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yawn yawn

# Manually add a boolean value to the database
printf 'bool: true' > `kdb file user/tests/yawn`

# Elektra stores boolean values as `0` and `1`
kdb get user/tests/yawn/bool
#> 1

# Undo modifications to the key database
kdb rm -r user/tests/yawn
sudo kdb umount user/tests/yawn
```

### Error Messages

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yawn yawn

# Manually add data containing syntax errors
printf -- 'Brutus: \n'         >  `kdb file user/tests/yawn`
printf -- '- Burst\n'          >> `kdb file user/tests/yawn` # Incorrect indentation
printf -- ' Holon : Anamnesis' >> `kdb file user/tests/yawn` # Either incorrect indentation or line above should be removed

# Try to retrieve data
kdb ls user/tests/yawn
# RET: 5

# Let us look at the error message more closely.
# Since the location of `config.yaml` depends on the current user and OS,
# we store the text before `config.yaml` as `user/tests/error/prefix`.
kdb set user/tests/error "$(2>&1 kdb ls user/tests/yawn)"
kdb set user/tests/error/prefix "$(kdb get user/tests/error | grep 'config.yaml' | head -1 | sed -E 's/(.*)config.yaml.*/\1/')"
# We also store the length of the prefix, so we can remove it from every
# line of the error message.
kdb set user/tests/error/prefix/length "$(kdb get user/tests/error/prefix | wc -c | sed 's/[ ]*//g')"

# Since we only want to look at the “reason” of the error, we
# remove the other part of the error message with `head` and `tail`.
kdb get user/tests/error | tail -n6 | cut -c"$(kdb get user/tests/error/prefix/length)"-
#> config.yaml:2:1: Syntax error on input “-”
#>                  - Burst
#>                  ^
#> config.yaml:3:19: Syntax error on input “end of map”
#>                    Holon : Anamnesis
#>                                     ^

# Fix syntax errors
printf -- 'Brutus: \n'         > `kdb file user/tests/yawn`
printf -- ' - Burst\n'        >> `kdb file user/tests/yawn`
printf -- 'Holon : Anamnesis' >> `kdb file user/tests/yawn`
kdb ls user/tests/yawn
#> user/tests/yawn/Brutus
#> user/tests/yawn/Brutus/#0
#> user/tests/yawn/Holon

# Undo modifications
kdb rm -r user/tests/error
kdb rm -r user/tests/yawn
sudo kdb umount user/tests/yawn
```

## Limitations

The plugin has the same limitations as [YAMBi ](../yambi/).
