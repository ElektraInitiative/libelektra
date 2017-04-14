- infos = Information about the mini plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/ini
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = experimental limited maintained nodep
- infos/metadata =
- infos/description = A minimal plugin for simple INI files

## mINI

The “maybe this is not INI” plugin (`mini`) is a very simple storage plugin loosely based on the [INI][] file format. Since this plugin **does not support sections** it might be more appropriate to say that it is based on the [.properties][] format, used in many Java applications.

[INI]: https://en.wikipedia.org/wiki/INI_file
[.properties]: https://en.wikipedia.org/wiki/.properties

## Examples

### Basic Usage

The following example shows basic usage of the `mini` plugin.

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

### Escaping

As with most configuration file formats, some characters carry special meaning. In the case of the `mini` plugin that character is the `=` sign, which separates keys from values. In most cases you do not need to care about the special meaning of the `=` sign though, since the plugin handles escaping and unescaping of `=` for you. The following example shows this behaviour.

```sh
kdb mount mini.ini /examples/mini mini

# Store a value and a key containing equal signs (`=`)
kdb set /examples/mini/=key value=
#> Using name user/examples/mini/=key
#> Create a new key user/examples/mini/=key with string value=

# The actual file contains escaped equal characters (`\=`)
kdb export /examples/mini mini
#> \=key=value\=

# However, if you retrieve values or keys you do not have to care about escaped values
kdb ls /examples/mini
#> user/examples/mini/=key
kdb get /examples/mini/=key
#> value=

# Undo modifications to the key database
kdb rm -r /examples/mini
kdb umount /examples/mini
```

## Limitations

This plugin only supports simple key-value based properties files without sections. mINI also does not support meta data. If you want a more feature complete plugin, then please take a look at the [ini plugin](../ini/). The example below shows some of the limitations of the plugin.

```sh
kdb mount mini.ini /examples/mini mini

# The plugin does not support sections or multi-line values
echo   '[section]'         >> `kdb file /examples/mini`
printf 'key="multi\nline"' >> `kdb file /examples/mini`

# mINI only reads the first line of the value with the name `key`, since
# the plugin assigns no special meaning to double or single quotes.
kdb ls /examples/mini 2> stderr.txt
#> user/examples/mini/key

# As we can see in the first two line of the standard error output below,
# mINI will inform us about lines it was unable to parse.
cat stderr.txt | grep 'Reason:' | sed 's/^[[:space:]]*//'
#> Reason: Line 1: “[section]” is not a valid key value pair
#> Reason: Line 3: “line"” is not a valid key value pair

# Unlike the `ini` and `ni` plugin, mINI does not support meta data.
kdb lsmeta /examples/mini
# RET:1

# The value of `key` also contains the double quote symbol, since mINI does
# not assign special meaning to quote characters.
kdb get /examples/mini/key
#> "multi

# Undo modifications to the key database
kdb rm -r /examples/mini
kdb umount /examples/mini
```
