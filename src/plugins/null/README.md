- infos = Information about null plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = code
- infos/needs =
- infos/placements = postgetstorage presetstorage
- infos/status = maintained nodep libc
- infos/description = Transforms null and empty values to text and back

## Introduction ##

This plugin is a filter plugin which allows Elektra to better deal with
null values and empty strings.

Elektra supports null values for binary keys.
They differ from an empty string substantially in how they are handled.
Some plugins that work on values are not aware of null values.
They would crash when they try
to access a value that is believed to be a string.

## Special Values ##

The plugin transcodes all null values to `@NULL`, all empty strings to
`@EMPTY`, and all strings starting with `@` to begin with `@@`.

## Usage ##

In order to use the `null` plugin you must add `null` to `info/needs`
in the plugin you wish to use.

Once `null` is included in `infos/needs`, the null plugin will filter
values for the storage plugin. As a result, the storage plugin will never come
across a null value or an empty string, instead it will be get strings
with `@NULL` and `@EMPTY` respectively.
