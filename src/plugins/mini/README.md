- infos = Information about the mini plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/ini
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = experimental limited maintained nodep preview unfinished
- infos/metadata =
- infos/description = A minimal plugin for simple INI files

## mINI

The minimal INI plugin (`mini`) is a very simple storage plugin based on the [INI][] file format.

[INI]: https://en.wikipedia.org/wiki/INI_file

## Examples

```sh
# Mount mini plugin to cascading namespace `/examples/mini`
kdb mount mini.ini /examples/mini mini

# Add two key value pairs to the database
kdb set /examples/mini/key value
#> Using name user/examples/mini/key
#> Create a new key user/examples/mini/key with string value
kdb set /examples/mini/mi/mi/mr beaker
#> Using name user/examples/mini/mi/mi/mr
#> Create a new key user/examples/mini/mi/mi/mr with string beaker

# Export our current configuration
kdb export /examples/mini mini
#> key=value
#> mi/mi/mr=beaker

# Manually add some values
echo "🔑 = 🦄"           >> `kdb file /examples/mini`
echo "level1/level2 = 👾" >> `kdb file /examples/mini`

# Now `/examples/mini` contains four key value pairs
kdb ls /examples/mini
#> user/examples/mini/key
#> user/examples/mini/level1/level2
#> user/examples/mini/mi/mi/mr
#> user/examples/mini/🔑

# Let us check if `/examples/mini/🔑` contains the correct value
kdb get "/examples/mini/🔑"
#> 🦄

# Undo modifications to the key database
kdb rm -r /examples/mini
kdb umount /examples/mini
```

## Limitations

This plugin only supports simple key-value based INI files without sections. If you want a more feature complete plugin, then please take a look at the [ini plugin](../ini/).
