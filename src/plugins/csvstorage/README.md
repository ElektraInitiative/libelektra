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
The default delimiter is `;` and will be used if `delimiter` is not set.

`useheader`
Tells the plugin to use the first line as a header if it's set to "1". The columns will get the coresponding names.
Ignore the first line if it's set to "-1" or treat the first line as a record if it's set to "0". 
If useheader is not set, or set to "0", the columns get namend #0,#1,...

`columns`
If this key is set the plugin will yield an error for every file that doesn't have exactly the amount of columns ans specified in `columns`.

`colNames`
Sets the column names. Only useable in combination with the `columns` key. The number of subkeys must match the number of columns.
```
columns = 2
colNames
colNames/#0 = col0Name
colNames/#1 = col1Name
```

## Example ##

`kdb mount test.csv /csv csvstorage delimiter=";" useheader="1"`

