- infos = Information about validation plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/placements = presetstorage
- infos/description = Validates key values using regular expressions

## Introduction ##

This plugin is a check plugin which checks string values of Keys using regular expressions.

## Usage ##

The validation plugin looks for two metakeys. `check/validation` gives a regular expression to check against. If it is present, `check/validation/message` may contain an optional humanly readable message that will be passed with the error information. 

The implementation consists of a loop checking for every key if it has the mentioned metakey. The check itself is done by the POSIX regular expression library with the interface `regcomp`, `regexec`, `regerror` and `regfree`. The ﬂag `REG_EXTENDED` is passed so that the regular expression will be compiled as an extended regular expression. `REG_NOSUB` gives a better perfomance and subexpressions cannot be used in this setup anyway. 

The plugin also exports the function `ksLookupRE()` that does a lookup in a KeySet using a regular expression. It starts from the current cursor of the KeySet and stops when the ﬁrst value matches. Finally this key is returned.

