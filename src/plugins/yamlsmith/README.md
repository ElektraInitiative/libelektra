- infos = Information about the yamlsmith plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = directoryvalue
- infos/provides = conv
- infos/recommends =
- infos/placements = setstorage
- infos/status = maintained specific unittest nodep preview experimental unfinished nodoc concept discouraged
- infos/metadata =
- infos/description = This plugin exports key sets in the YAML format

# YAML Smith

## Examples

### Mappings

```sh
# Add some key-value pairs to the database
kdb set user/tests/yamlsmith/key value
kdb set user/tests/yamlsmith/time 'will die and love will burrow it'
kdb set user/tests/yamlsmith/level1/one 'I'
kdb set user/tests/yamlsmith/level1/two 'II'

# Export data using YAML Smith
kdb export user/tests/yamlsmith yamlsmith
#> key:
#>   value
#> level1:
#>   one:
#>     I
#>   two:
#>     II
#> time:
#>   will die and love will burrow it

# Undo modifications
kdb rm -r user/tests/yamlsmith
```

## Arrays

```sh
kdb set user/tests/yamlsmith/low
kdb set user/tests/yamlsmith/low/#0 'You bought some sweet, sweet, sweet, sweet sunflowers'
kdb set user/tests/yamlsmith/low/#1 'And gave them'
kdb set user/tests/yamlsmith/low/#2 'To the night'

kdb export user/tests/yamlsmith yamlsmith
#> low:
#>   -
#>     You bought some sweet, sweet, sweet, sweet sunflowers
#>   -
#>     And gave them
#>   -
#>     To the night

# Undo modifications
kdb rm -r user/tests/yamlsmith
```

## Limitations

This plugin supports only **a very limited** subset of YAML.
