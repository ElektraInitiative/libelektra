- infos = Information about the enum plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/placements = presetstorage
- infos/description =

## Introduction ##

The Enum plugin checks string values of Keys by comparing it against a list of valid values.

## Usage ##

The plugin checks every Key in the Keyset for the Metakey `check/enum` containing a list with the syntax `['string1', 'string2', 'string3', ..., 'stringN']` and compares each value with the string value of the Key. If no match is found an error is returned.

## Dependencies ##

The POSIX regular expression library is used to parse the metakey and split the list.
