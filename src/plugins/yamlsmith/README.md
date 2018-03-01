- infos = Information about the yamlsmith plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = conv
- infos/recommends =
- infos/placements = setstorage
- infos/status = maintained specific unittest nodep preview experimental unfinished nodoc concept discouraged
- infos/metadata =
- infos/description = This plugin exports key sets in the YAML format

# YAML Smith

## Example

```sh
# Add some key-value pairs to the database
kdb set user/examples/yamlsmith/key value
kdb set user/examples/yamlsmith/time 'will die and love will burrow it'

# Export data using YAML Smith
kdb export user/examples/yamlsmith yamlsmith
#> key: value
#> time: will die and love will burrow it

# Undo modifications
kdb rm -r user/examples/yamlsmith
```

## Limitations

This plugin currently does nothing useful.
