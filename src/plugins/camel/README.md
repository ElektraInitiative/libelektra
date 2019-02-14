- infos = Information about the camel plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/camel
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained shelltest unittest nodep preview unfinished concept discouraged
- infos/metadata =
- infos/description = A very basic plugin that reads and writes a very small subset of YAML

# Camel

## Introduction

This plugin reads configuration data specified in a **very limited** subset of the data serialization language [YAML](http://www.yaml.org).

## Examples

### Basic Usage

```sh
# Mount camel plugin to cascading namespace `/tests/camel`
sudo kdb mount config.yaml /tests/camel camel

kdb set /tests/camel/key value
kdb get /tests/camel/key
#> value

kdb set /tests/camel/kittens "warm & fuzzy"
kdb get /tests/camel/kittens
#> warm & fuzzy

kdb set /tests/camel/empty ""

kdb export /tests/camel camel
#> {
#>   "empty" : ""
#> , "key" : "value"
#> , "kittens" : "warm & fuzzy"
#> }

kdb rm -r /tests/camel
sudo kdb umount /tests/camel
```

## Limitations

Currently this plugin **should not be used by anyone**.
