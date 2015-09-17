- infos = Information about the csvstorage plugin is in keys below
- infos/author = Thomas Waser
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = parses csv files

## Introduction ##

This plugin allowes Elektra to read and write CSV files.

## Configuration ##

`config/delimiter`
Tells the plugin what delimiter is used in the file.
The default delimiter is ; and will be used if config/delimiter is not set.

`config/useheader = 1`
Tells the plugin to use the first line as a header. The columns will get the
coresponding names.

`config/outputdelim`
Tells the plugin what delimiter should be used for output.
If not set, ; will be used

## Example ##

`kdb mount test.csv /csv csvstorage delimiter=";" useheader=1`

