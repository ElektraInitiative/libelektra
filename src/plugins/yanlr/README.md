- infos = Information about the yanlr plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/unknown
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = maintained preview experimental unfinished concept discouraged nodoc
- infos/metadata =
- infos/description = This plugin shows how you can use ANTLR to generate a storage plugin

# Yan LR

## Example

```sh
# Mount plugin to cascading namespace `/examples/yanlr`
sudo kdb mount config.file /examples/yanlr yanlr

# Undo modifications to the key database
sudo kdb umount /examples/yanlr
```

## Limitations

- The plugin currently does not provide any useful functionality
