- infos = Information about the mini plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/needs = ccode
- infos/provides = storage/properties
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = tested/unit tested/shell nodep
- infos/features/storage = limited
- infos/metadata =
- infos/description = A minimal plugin for properties files

# mINI

The “maybe this is not INI” plugin (`mini`) is a very simple storage plugin loosely based on the [INI][] file format. Since this plugin **does not support sections** it might be more appropriate to say that it is based on the [.properties][] format, used in many Java applications.

[ini]: https://en.wikipedia.org/wiki/INI_file
[.properties]: https://en.wikipedia.org/wiki/.properties

## Examples

### Basic Usage

The following example shows basic usage of the `mini` plugin.

```sh
# Mount mini plugin to `user:/tests/mini`
sudo kdb mount mini.ini user:/tests/mini mini

# Add two key value pairs to the database
kdb set user:/tests/mini/key value
#> Create a new key user:/tests/mini/key with string "value"
kdb set user:/tests/mini/mi/mi/mr beaker
#> Create a new key user:/tests/mini/mi/mi/mr with string "beaker"

# Export our current configuration
kdb export user:/tests/mini mini
#> key=value
#> mi/mi/mr=beaker

# Manually add some values
echo "🔑 = 🦄"           >> `kdb file user:/tests/mini`
echo "level1/level2 = 👾" >> `kdb file user:/tests/mini`

# Now `user:/tests/mini` contains four key value pairs
kdb ls user:/tests/mini
#> user:/tests/mini/key
#> user:/tests/mini/level1/level2
#> user:/tests/mini/mi/mi/mr
#> user:/tests/mini/🔑

# Let us check if `user:/tests/mini/🔑` contains the correct value
kdb get "user:/tests/mini/🔑"
#> 🦄

# Undo modifications to the key database
kdb rm -r user:/tests/mini
sudo kdb umount user:/tests/mini
```

### Escaping

As with most configuration file formats, some characters carry special meaning. In the case of the `mini` plugin that character are

1. the `=` sign, which separates keys from values and
2. `#` and `;`, the characters that denote a comment.

In case of **key values** you do not need to care about the special meaning of these characters most of the time, since the plugin handles escaping and unescaping of them for you. Since mINI use the backslash character (`\`) to escape values, the backspace character will be escaped too (`\\`). The following example shows the escaping behavior.

```sh
sudo kdb mount mini.ini user:/tests/mini mini

# Store a value containing special characters
kdb set user:/tests/mini/key ';#=\'
#> Create a new key user:/tests/mini/key with string ";#=\"

# The actual file contains escaped version of the characters
kdb file user:/tests/mini | xargs cat
#> key=\;\#\=\\

# However, if you retrieve the value you do not have to care
# about the escaped characters
kdb get user:/tests/mini/key
#> ;#=\

# If we do not escape the `;` and `#` characters, then they
# donate a comment.
echo 'background = \#0F0F0F ; Background color' >> `kdb file user:/tests/mini`
echo 'foreground = \#FFFFFF # Foreground color' >> `kdb file user:/tests/mini`
kdb get user:/tests/mini/background
#> #0F0F0F
kdb get user:/tests/mini/foreground
#> #FFFFFF

# Undo modifications to the key database
kdb rm -r user:/tests/mini
sudo kdb umount user:/tests/mini
```

In the case of **key names** you **must not use any of the characters mentioned above** (`;`, `#` and `=`) at all. Otherwise the behavior of the plugin will be **undefined**.

## Limitations

This plugin only supports simple key-value based properties files without sections. mINI also does not support metadata. If you want a more feature complete plugin, then please take a look at the [toml plugin](../toml/). The example below shows some of the limitations of the plugin.

```sh
sudo kdb mount mini.ini user:/tests/mini mini

# The plugin does not support sections or multi-line values
echo   '[section]'         >> `kdb file user:/tests/mini`
printf 'key="multi\nline"' >> `kdb file user:/tests/mini`

# mINI only reads the first line of the value with the name `key`, since
# the plugin assigns no special meaning to double or single quotes.
kdb ls user:/tests/mini 2> stderr.txt
#> user:/tests/mini/key

# As we can see in the first two line of the standard error output below,
# mINI will inform us about lines it was unable to parse.
cat stderr.txt | grep -oE 'Line [[:digit:]]+.*' | sed 's/^[[:space:]]*//'
#> Line 1: '[section]' is not a valid key value pair
#> Line 3: 'line"' is not a valid key value pair

# Unlike the `ini` and `ni` plugin, mINI does not support metadata.
kdb meta-set user:/tests/mini foo bar
# RET: 5

kdb meta-ls user:/tests/mini
# RET: 1

# The value of `key` also contains the double quote symbol, since mINI does
# not assign special meaning to quote characters.
kdb get user:/tests/mini/key
#> "multi

# Undo modifications
rm stderr.txt
kdb rm -r user:/tests/mini
sudo kdb umount user:/tests/mini
```
