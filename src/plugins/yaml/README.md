- infos = Information about the yaml plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements =
- infos/status = maintained preview experimental unfinished concept discouraged
- infos/metadata =
- infos/description = A very basic plugin that reads and writes a very small subset of YAML

## Introduction

This plugin reads configuration data specified in a **very limited** subset of  the data serialization language [YAML](http://www.yaml.org).

## Examples

### Basic Usage

```sh
# Mount mini plugin to cascading namespace `/examples/yaml`
kdb mount config.yaml /examples/yaml yaml

kdb set /examples/yaml/key value
kdb get /examples/yaml/key
#> value

kdb set /examples/yaml/kittens "warm & fuzzy"
kdb get /examples/yaml/kittens
#> warm & fuzzy

kdb export /examples/yaml yaml
#> {
#>   "key" : "value"
#> , "kittens" : "warm & fuzzy"
#> }

kdb rm -r /examples/yaml
kdb umount /examples/yaml
```

## Limitations

Currently this plugin **should not be used by anyone**.
