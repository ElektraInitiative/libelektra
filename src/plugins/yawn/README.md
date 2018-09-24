- infos = Information about the yawn plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = directoryvalue yamlsmith
- infos/provides = storage/yaml
- infos/recommends =
- infos/placements = getstorage
- infos/status = maintained unittest preview experimental unfinished nodoc concept discouraged
- infos/metadata =
- infos/description = This storage plugin use a Early parser to read YAML files

# YAwn

## Introduction

This plugin uses [YAEP](https://github.com/vnmakarov/yaep) to  parse the [YAML](http://yaml.org) serialization format.

## Dependencies

This plugin requires [YAEP](https://github.com/vnmakarov/yaep#installing).

## Examples

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/yawn yawn

kdb set user/tests/yawn/ing 'keeps you alert'
kdb get user/tests/yawn/ing
#> keeps you alert

# Undo modifications
sudo kdb umount user/tests/yawn
```

## Limitations

This plugin does not do anything useful yet.
