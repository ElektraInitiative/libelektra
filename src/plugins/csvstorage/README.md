- infos = Information about the csvstorage plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = parses csv files

## Introduction ##

This plugin allowes Elektra to read and write CSV files.

## Configuration ##

`delimiter`
Tells the plugin what delimiter is used in the file.
The default delimiter is `,` and will be used if `delimiter` is not set.

`header`
Tells the plugin to use the first line as a header if it's set to "colname". The columns will get the corresponding names.
Skip the first line if it's set to "skip" or treat the first line as a record if it's set to "record".
If `header` is not set, or set to "record", the columns get named #0,#1,... (array key naming)

`columns`
If this key is set the plugin will yield an error for every file that doesn't have exactly the amount of columns ans specified in `columns`.

`columns/names`
Sets the column names. Only usable in combination with the `columns` key. The number of subkeys must match the number of columns.
Conflicts with usage of `header`.

## Examples ##

First line should determine the headers:

	kdb mount test.csv /csv csvstorage "delimiter=;,header=colname,columns=2,columns/names,columns/names/#0=col0Name,columns/names/#1=col1Name"


## Limitations ##

- Does not work on file streams (e.g. `kdb import` without file)
- Fails without error on `kdb mount test.csv /csv csvstorage "delimiter=;,columns=2,columns/names=,columns/names/#0=col0Name,columns/names/#1=col1Name"`
