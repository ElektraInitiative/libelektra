- infos = Information about the yamlsmith plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = directoryvalue
- infos/provides =
- infos/recommends =
- infos/placements = setstorage
- infos/status = maintained specific unittest nodep preview experimental unfinished nodoc concept discouraged
- infos/metadata =
- infos/description = This plugin exports key sets in the YAML format

# YAML Smith

## Examples

### Scalar

```sh
# Save a single key-value pair
kdb set user:/tests/yamlsmith/text 'Pattern Against User'

# Only export the value
kdb export user:/tests/yamlsmith/text yamlsmith
#> "Pattern Against User"

# Check that the plugin supports boolean values correctly
kdb set user:/tests/yamlsmith/boolean 0
kdb export user:/tests/yamlsmith/boolean yamlsmith
#> false

# Undo modifications
kdb rm -r user:/tests/yamlsmith
```

### Mappings

```sh
# Add some key-value pairs to the database
kdb set user:/tests/yamlsmith/key value
kdb set user:/tests/yamlsmith/time 'will die and love will burrow it'
kdb set user:/tests/yamlsmith/level1/one 'I'
kdb set user:/tests/yamlsmith/level1/two 'II'

# Export data using YAML Smith
kdb export user:/tests/yamlsmith yamlsmith
#> key:
#>   "value"
#> level1:
#>   one:
#>     "I"
#>   two:
#>     "II"
#> time:
#>   "will die and love will burrow it"

# Undo modifications
kdb rm -r user:/tests/yamlsmith
```

## Arrays

### Simple Array

```sh
kdb set user:/tests/yamlsmith/low
kdb set user:/tests/yamlsmith/low/#0 'You bought some sweet, sweet, sweet, sweet sunflowers'
kdb set user:/tests/yamlsmith/low/#1 'And gave them'
kdb set user:/tests/yamlsmith/low/#2 'To the night'

kdb export user:/tests/yamlsmith yamlsmith
#> low:
#>   -
#>     "You bought some sweet, sweet, sweet, sweet sunflowers"
#>   -
#>     "And gave them"
#>   -
#>     "To the night"

# Undo modifications
kdb rm -r user:/tests/yamlsmith
```

### Multiple Arrays

```sh
kdb set user:/tests/yamlsmith/arrays/Elliott/Smith/#0 XO
kdb set user:/tests/yamlsmith/arrays/Elliott/Smith/#1 'Figure 8'
kdb set user:/tests/yamlsmith/arrays/The/Smiths/#0 'The Queen Is Dead'

kdb export user:/tests/yamlsmith yamlsmith
#> arrays:
#>   Elliott:
#>     Smith:
#>       -
#>         "XO"
#>       -
#>         "Figure 8"
#>   The:
#>     Smiths:
#>       -
#>         "The Queen Is Dead"

# Undo modifications
kdb rm -r user:/tests/yamlsmith
```

## Limitations

- This plugin supports only **a very limited** subset of YAML.
- YAML Smith **does not support**

  - **binary values**,
  - **non-contiguous arrays** and
  - **special characters**

  .
