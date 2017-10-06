- infos = Information about the camel plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/camel
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained preview experimental unfinished concept discouraged shelltest unittest
- infos/metadata =
- infos/description = A very basic plugin that reads and writes a very small subset of YAML

## Introduction

This plugin reads configuration data specified in a **very limited** subset of  the data serialization language [YAML](http://www.yaml.org).

## Examples

### Basic Usage

```sh
# Mount yaml plugin to cascading namespace `/examples/camel`
sudo kdb mount config.yaml /examples/camel camel

kdb set /examples/camel/key value
kdb get /examples/camel/key
#> value

kdb set /examples/camel/kittens "warm & fuzzy"
kdb get /examples/camel/kittens
#> warm & fuzzy

kdb set /examples/camel/empty ""

kdb export /examples/camel camel
#> {
#>   "empty" : ""
#> , "key" : "value"
#> , "kittens" : "warm & fuzzy"
#> }

kdb rm -r /examples/camel
sudo kdb umount /examples/camel
```

## Limitations

Currently this plugin **should not be used by anyone**.
