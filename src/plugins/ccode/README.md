- infos = Information about ccode plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = code
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained unittest nodep libc configurable
- infos/description = Decoding/Encoding engine which escapes unwanted characters.

## Introduction

When writing a C string in C code some characters cannot be expressed
directly but have a special one letter abbreviation.
The ccode plugin allows us to map any single escaped
character to be replaced by another single character and vice versa.
The user can configure this mapping.

## Examples

```sh
# Mount `tcl` storage plugin together with the required `base64` plugin.
# We use the `ccode` plugin to escape special characters.
sudo kdb mount config.tcl user/tests/ccode tcl ccode base64

# Add a key value containing newline characters
kdb set user/tests/ccode/multiline "`printf 'one\ntwo\nthree'`"
# By default the plugin uses `\n` to escape newline characters
grep 'multiline' `kdb file user/tests/ccode/multiline` | sed 's/[[:space:]]*//'
#> user/tests/ccode/multiline = one\ntwo\nthree

# The `ccode` plugin escapes and unescapes the data. The `tcl` plugin
# returns the unescaped values.
kdb get user/tests/ccode/multiline
#> one
#> two
#> three

# Write and read a key value containing a tab character
kdb set user/tests/ccode/tab 'Tab	Fabulous'
kdb get user/tests/ccode/tab
#> Tab	Fabulous

# Undo modifications to database
kdb rm -r user/tests/ccode
sudo kdb umount user/tests/ccode
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
