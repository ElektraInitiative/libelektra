- infos = Information about null plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs = 
- infos/provides = 
- infos/placements = postgetstorage presetstorage
- infos/description = Avoids null and empty values

## Introdcution ##

This plugin is a filter plugin which allows Elektra to better deal with null values and empty strings.

## Special Values ## 

The plugin transcodes all null values to `@NULL`, all empty strings to `@EMPTY`, and all strings starting with `@` to begin with `@@`.

## Usage ##

In order to use the `null` plugin you must add `null` to `info/needs` in the plugin you wish to use. 

Once `null` is included in `infos/needs`, the null plugin will filter values for the other plugin. As a result, the plugin will never come across a null value or an empty string, instead it will be presented with `@NULL` and `@EMPTY` respectively. 
