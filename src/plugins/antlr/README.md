- infos = Information about the antlr plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/unknown
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained preview experimental unfinished concept discouraged nodoc
- infos/metadata =
- infos/description = This plugin shows how you can use ANTLR to generate a storage plugin

# ANTLR

## Example

```sh
# Mount plugin to cascading namespace `/examples/antlr`
sudo kdb mount config.file /examples/antlr antlr

# Undo modifications to the key database
sudo kdb umount /examples/antlr
```

## Limitations

- The plugin currently does not provide any useful functionality
