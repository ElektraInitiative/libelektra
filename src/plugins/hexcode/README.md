- infos = Information about hexcode plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = code
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained tested/unit nodep configurable
- infos/description = Decoding/Encoding engine which escapes unwanted characters.

## Introduction

This code plugin translates each unwanted character into a two cypher
hexadecimal character. The escape character itself always needs to be
encoded, otherwise the plugin would try to interpret the following two
characters in the text as a hexadecimal sequence.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Restrictions

- The escape character itself always needs to be encoded, otherwise
  the plugin would try to interpret the following two characters in the
  text as a hexadecimal sequence.
- The length of the resulting string increases. In the worst case the
  hexcode plugin makes the value three times larger.

## Example

Consider the following _value_ of an key:

```ini
value=abc xyz
```

Assuming the escape character is % the input would be encoded to:

```
value%3Dabc%20xyz
```

The disadvantage is that the length of the resulting string increases.
In the worst case
the hexcode plugin makes the value three times larger.

## Usage

Add `hexcode` to `infos/needs` for any plugin that you want to be filtered
by hexcode.

Then, additionally define all characters you need to be escaped below
`config/needs/chars` in your contract, e.g:

```ini
config/needs/chars/20 = 61
```

to transform a space (dec 20) to the escaped letter a (dec 61).

The escape letter itself can be changed by setting:

```
config/needs/escape
```
