- infos = Information about validation plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = presetstorage
- infos/status = maintained tested/unit nodep
- infos/metadata = check/validation check/validation/message check/validation/ignorecase check/validation/match check/validation/invert check/validation/type
- infos/description = Validates key values using regular expressions

## Introduction

This plugin is a check plugin which checks string values of Keys using
regular expressions.

## Usage

The validation plugin looks for two metakeys. `check/validation`
gives a regular expression to check against. If it is present,
`check/validation/message` may contain an optional humanly readable
message that will be passed with the error information.

Important:
To validate against the whole string, you have to start the regular
expression with `^` and end it with `$`. Otherwise expressions that
e.g. match the empty string, always return true.
Alternatively, you can use `check/validation/match=LINE`.

## Configuration

Metadata can be supplied to configure the validation:

- `check/validation/match`: You can check against `LINE`, `WORD` or `ANY`
- `check/validation/ignorecase`: If you want to ignore case.
- `check/validation/invert`: If you want to invert match.

## Implementation

The implementation consists of a loop checking for every key if it has
the mentioned metakey. The check itself is done by the POSIX regular
expression library with the interface `regcomp`, `regexec`, `regerror`
and `regfree`. The flag `REG_EXTENDED` is passed so that the regular
expression will be compiled as an extended regular expression. `REG_NOSUB`
gives a better performance and subexpressions cannot be used in this
setup anyway.

## Exported Methods

The plugin also exports the function `ksLookupRE()` that does a lookup in
a KeySet using a regular expression. It starts from the current cursor
of the KeySet and stops when the first value matches. Finally, this key
is returned.
