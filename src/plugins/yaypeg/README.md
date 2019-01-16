- infos = Information about the yaypeg plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = directoryvalue yamlsmith
- infos/provides = storage/yaml
- infos/recommends =
- infos/placements = getstorage
- infos/status = maintained preview experimental unfinished nodoc concept discouraged
- infos/metadata =
- infos/description = This storage plugin uses a PEG based parser to read YAML files

# YAy PEG

The YAy PEG plugin use a parser based on [PEGTL](https://github.com/taocpp/PEGTL) to convert YAML data to Elektra’s `KeySet` type.

## Dependencies

This plugin requires [PEGTL](https://github.com/taocpp/PEGTL/blob/2.7.x/doc/Installing-and-Using.md) `2.7.1`. Previous versions of the
library might work too. However, neither the current development version of PEGTL (`3.0`), nor the latest version of PEGTL `2.7.x` can be
used by the plugin because of the issue referenced [here](https://github.com/taocpp/PEGTL/issues/143).

## Examples

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yaypeg yaypeg

kdb set user/tests/yaypeg/key value
#> Create a new key user/tests/yaypeg/key with string "value"

kdb get /tests/yaypeg/key
#> value

# Undo modifications
kdb rm -r user/tests/yaypeg
sudo kdb umount user/tests/yaypeg
```

## Limitations

This plugin currently does nothing useful at all.
