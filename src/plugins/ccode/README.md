- infos = Information about ccode plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = code
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = unittest nodep libc discouraged
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

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

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
