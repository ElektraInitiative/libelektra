- infos = Information about the yaypeg plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = directoryvalue yamlsmith
- infos/provides = storage/yaml
- infos/recommends =
- infos/placements = getstorage
- infos/status = maintained unittest preview experimental unfinished nodoc concept discouraged
- infos/metadata =
- infos/description = This storage plugin uses a PEG based parser to read YAML files

# YAy PEG

The YAy PEG plugin use a parser based on [PEGTL](https://github.com/taocpp/PEGTL) to convert YAML data to Elektra’s `KeySet` type.

## Dependencies

This plugin requires [PEGTL](https://github.com/taocpp/PEGTL/blob/2.7.x/doc/Installing-and-Using.md) `2.7.1`. Previous versions of the
library might work too. However, neither the current development version of PEGTL (`3.0`), nor the latest version of PEGTL `2.7.x` can be
used by the plugin because of the issue referenced [here](https://github.com/taocpp/PEGTL/issues/143).

## Examples

### Mappings

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yaypeg yaypeg

# Add some values
kdb set user/tests/yaypeg/movements/deadly 'Dull'
kdb set user/tests/yaypeg/movements/deep 'Red'

# Manually add a key
printf 'Rosalía: El Mal Querer' >> `kdb file user/tests/yaypeg`

# List keys
kdb ls user/tests/yaypeg
#> user/tests/yaypeg/Rosalía
#> user/tests/yaypeg/movements/deadly
#> user/tests/yaypeg/movements/deep

# Retrieve a value
kdb get user/tests/yaypeg/Rosalía
#> El Mal Querer

# Undo modifications
kdb rm -r user/tests/yaypeg
sudo kdb umount user/tests/yaypeg
```

### Arrays

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yaypeg yaypeg

# Manually add some values
printf 'ponies:\n'               >  `kdb file user/tests/yaypeg`
printf ' - Flutter Shy # Yay!\n' >> `kdb file user/tests/yaypeg`
printf ' - Pinkie Pie\n'         >> `kdb file user/tests/yaypeg`
printf ' - Maud Pie'             >> `kdb file user/tests/yaypeg`

# List array keys
kdb ls user/tests/yaypeg
#> user/tests/yaypeg/ponies
#> user/tests/yaypeg/ponies/#0
#> user/tests/yaypeg/ponies/#1
#> user/tests/yaypeg/ponies/#2

# Retrieve last array entry
kdb get user/tests/yaypeg/ponies/$(kdb getmeta user/tests/yaypeg/ponies array)
#> Maud Pie

# Undo modifications
kdb rm -r user/tests/yaypeg
sudo kdb umount user/tests/yaypeg
```

## Limitations

The plugin has the same limitations as [YAMBi ](../yambi/).
