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
printf 'Rosalía: El#Mal#Querer' >> `kdb file user/tests/yaypeg`

# List keys
kdb ls user/tests/yaypeg
#> user/tests/yaypeg/Rosalía
#> user/tests/yaypeg/movements/deadly
#> user/tests/yaypeg/movements/deep

# Retrieve a value
kdb get user/tests/yaypeg/Rosalía
#> El#Mal#Querer

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
printf ' - Fluttershy # Yay!\n' >> `kdb file user/tests/yaypeg`
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

### Error Messages

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yaypeg yaypeg

# Manually add syntactically incorrect data
printf 'Fluttershy:\n'           >  `kdb file user/tests/yaypeg`
printf 'I’d like to be a tree\n' >> `kdb file user/tests/yaypeg` # Incorrect indentation

kdb ls user/tests/yaypeg
# RET: 5
# STDERR: .*config.yaml:2:0: Incomplete document, expected “end of file”.*

# Let us look at the error message more closely.
# Since the location of `config.yaml` depends on the current user and OS,
# we store the text before `config.yaml` as `user/tests/error/prefix`.
kdb set user/tests/error "$(2>&1 kdb ls user/tests/yaypeg)"
kdb set user/tests/error/prefix "$(kdb get user/tests/error | grep 'config.yaml' | head -1 | sed -E 's/(.*)config.yaml.*/\1/')"
# We also store the length of the prefix, so we can remove it from every
# line of the error message.
kdb set user/tests/error/prefix/length "$(kdb get user/tests/error/prefix | wc -c | sed 's/[ ]*//g')"

# Since we only want to look at the “reason” of the error, we
# remove the other part of the error message with `head` and `tail`.
kdb get user/tests/error | tail -n8 | head -n3 | cut -c"$(kdb get user/tests/error/prefix/length)"-
#> config.yaml:2:0: Incomplete document, expected “end of file”
#>                  I’d like to be a tree
#>                  ^

# Fix syntax error
printf 'Fluttershy:\n'             >  `kdb file user/tests/yaypeg`
printf '  I’d like to be a tree\n' >> `kdb file user/tests/yaypeg`

kdb get user/tests/yaypeg/Fluttershy
#> I’d like to be a tree

# Add double quoted scalar with missing closing quote character
printf '"nothing,nowhere.' > `kdb file user/tests/yaypeg`
kdb ls user/tests/yaypeg
# RET: 5
# STDERR: .*config.yaml:1:17: Missing closing double quote for flow scalar.*

# Fix syntax error
printf '"nothing,nowhere."' > `kdb file user/tests/yaypeg`

# Undo modifications
kdb rm -r user/tests/error
kdb rm -r user/tests/yaypeg
sudo kdb umount user/tests/yaypeg
```

## Limitations

The plugin has the same limitations as [YAMBi ](../yambi/).
