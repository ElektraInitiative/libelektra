- infos = Information about ccode plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = code
- infos/placements = postgetstorage presetstorage
- infos/description = Decoding/Encoding engine which escapes unwanted characters.

## Introduction ##

When writing a C string in C code some characters cannot be expressed
directly but have a special one letter abbreviation.
The ccode plugin allows us to map any single escaped
character to be replaced by another single character and vice versa.
The user can configure this mapping.

## Restrictions ##

This method of encoding characters is not as powerful as the hexcode plugin in terms of reduction.
The hexcode plugin allows reduction of the character set to '0'-'9', 'a'-'f' and one escape character.
So it can represent any key value with only 17 characters.
On the other hand, ccode cannot reduce the set more than by half.

So when all control characters and non-ASCII characters need to vanish,
it cannot be done with the ccode plugin.
But it is perfectly suitable to reduce by some characters.
The advantages are that the size only doubles in the worst case and that
it is much easier to read.

## C ##

In the language C, following escape characters are present.

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

### Contract ###

Add `ccode` to `infos/needs` for any plugin that you want to be filtered by ccode.
