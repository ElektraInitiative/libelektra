- infos = Information about the yamlsmith plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/yaml
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained specific unittest nodep preview experimental unfinished nodoc concept discouraged
- infos/metadata =
- infos/description = This plugin exports key sets in the YAML format

# YAML Smith

## Example

```sh
# Mount plugin
sudo kdb mount config.yaml user/examples/yamlsmith yamlsmith

# Unmount plugin
sudo kdb umount user/examples/yamlsmith
```

## Limitations

This plugin currently does nothing useful.
