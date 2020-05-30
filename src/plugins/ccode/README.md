- infos = Information about ccode plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = code
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest nodep libc configurable
- infos/description = Decoding/Encoding engine which escapes unwanted characters.

# CCode

## Introduction

The `ccode` plugin replaces (escapes) any special characters with two characters:

- an escape character (default: `\`) and
- another character representing the escaped character (e.g `n` for newline)

before writing a `KeySet`. The plugin undoes this modification after reading a `KeySet`.

CCode provides a reasonable default configuration, using the usual escape sequences
for C strings (e.g. `\n` for newline, `\t` for tab). You can also configure the escape character
(`/escape`) and the mapping for special characters (`chars`).

## Examples

### Default Configuration

```sh
# Mount `tcl` storage plugin together with the required `base64` plugin.
# We use the `ccode` plugin to escape special characters.
sudo kdb mount config.tcl user:/tests/ccode tcl ccode base64

# Add a key value containing newline characters
kdb set user:/tests/ccode/multiline "`printf 'one\ntwo\nthree'`"
# By default the plugin uses `\n` to escape newline characters
grep 'multiline' `kdb file user:/tests/ccode` | sed 's/[[:space:]]*//'
#> multiline = one\ntwo\nthree

# The `ccode` plugin escapes and unescapes the data. The `tcl` plugin
# returns the unescaped values.
kdb get user:/tests/ccode/multiline
#> one
#> two
#> three

# Write and read a key value containing a tab character
kdb set user:/tests/ccode/tab 'Tab	Fabulous'
kdb get user:/tests/ccode/tab
#> Tab	Fabulous

# The plugin also escapes special characters inside key names
kdb set 'user:/tests/ccode/tab/t\\ta	b' 'Escaped Tabs'
grep 'tab/' `kdb file user:/tests/ccode` | sed 's/[[:space:]]*//'
#> tab/t\\\\ta\\tb = Escaped Tabs

# Undo modifications to database
kdb rm -r user:/tests/ccode
sudo kdb umount user:/tests/ccode
```

### Custom Configuration

```sh
# We use `%` (hex: `25`) as escape character and map the space character (hex: `20`) to the character `_` (hex `5f`).
sudo kdb mount config.tcl user:/tests/ccode tcl base64 ccode escape=25 chars/20=5f

kdb set user:/tests/ccode/spaces 'one two three'

grep 'space' `kdb file user:/tests/ccode/spaces` | sed 's/[[:space:]]*//'
#> spaces = one%_two%_three

kdb get user:/tests/ccode/spaces
#> one two three

# Undo modifications to database
kdb rm -r user:/tests/ccode
sudo kdb umount user:/tests/ccode
```

## Restrictions

This method of encoding characters is not as powerful as the hexcode plugin in terms of reduction.
The hexcode plugin allows reduction of the character set to '0'-'9', 'a'-'f' and one escape character.
So it can represent any key value with only 17 characters.
On the other hand, ccode cannot reduce the set more than by half.

So when all control characters and non-ASCII characters need to vanish,
it cannot be done with the ccode plugin.
But it is perfectly suitable to reduce by some characters.
The advantages are that the size only doubles in the worst case and that
it is much easier to read.

## C

In the C language, the following escape characters are present.

- `b`: backspace, hex: 08
- `t`: horizontal tab, hex: 09
- `n`: new line feed, hex: 0A
- `v`: vertical tab, hex: 0B
- `f`: form feed, hex: 0C
- `r`: carriage return, hex: 0D
- `\\`: back slash, hex: 5C
- `'`: single quote, hex: 27
- `"`: double quote, hex: 22
- `0`: null, hex: 00

This is also the default mapping.

### Contract

Add `ccode` to `infos/needs` for any plugin that you want to be filtered by ccode.
