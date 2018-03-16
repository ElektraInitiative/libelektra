- infos = Information about the csvstorage plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/csv
- infos/needs =
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = productive maintained unittest nodep libc configurable
- infos/description = parses csv files

## Introduction

This plugin allows you to read and write CSV files within Elektra.
It aims to be compatible with RFC 4180.
Rows and columns are written in Elektra arrays (`#0`, `#1`,..).
Using configuration you can give columns a name.

## Configuration

`delimiter`
Tells the plugin what delimiter is used in the file.
The default delimiter is `,` and will be used if `delimiter` is not set.

`header`
Tells the plugin to use the first line as a header if it's set to "colname". The columns will get the corresponding names.
Skip the first line if it's set to "skip" or treat the first line as a record if it's set to "record".
If `header` is not set, or set to "record", the columns get named #0,#1,... (array key naming)

`columns`
If this key is set the plugin will yield an error for every file that doesn't have exactly the amount of columns as specified in `columns`.

`columns/names`
Sets the column names. Only usable in combination with the `columns` key. The number of subkeys must match the number of columns.
Conflicts with usage of `header`.

`columns/index`
Use the value of the column given as argument to index the records instead of the record number.

## Examples

First line should determine the headers:

    kdb mount test.csv /csv csvstorage "delimiter=;,header=colname,columns=2,columns/names,columns/names/#0=col0Name,columns/names/#1=col1Name"

### Usage

The example below shows how you can use this plugin to read and write CSV files.

```sh
# Mount plugin to cascading namespace `/examples/csv`
# We use the column names from the first line of the
# config file as key names
sudo kdb mount config.csv /examples/csv csvstorage  "header=colname,columns/names/#0=col0Name,columns/names/#1=col1Name"

# Add some data
printf 'band,album\n'                           >> `kdb file /examples/csv`
printf 'Converge,All We Love We Leave Behind\n' >> `kdb file /examples/csv`
printf 'mewithoutYou,Pale Horses\n'             >> `kdb file /examples/csv`
printf 'Kate Tempest,Everybody Down\n'          >> `kdb file /examples/csv`

kdb ls /examples/csv
#> user/examples/csv/#0
#> user/examples/csv/#0/album
#> user/examples/csv/#0/band
#> user/examples/csv/#1
#> user/examples/csv/#1/album
#> user/examples/csv/#1/band
#> user/examples/csv/#2
#> user/examples/csv/#2/album
#> user/examples/csv/#2/band
#> user/examples/csv/#3
#> user/examples/csv/#3/album
#> user/examples/csv/#3/band

# The first array element contains the column names
kdb get /examples/csv/#0/band
#> band
kdb get /examples/csv/#0/album
#> album

# Retrieve data from the last entry
kdb get /examples/csv/#3/album
#> Everybody Down
kdb get /examples/csv/#3/band
#> Kate Tempest

# Change an existing item
kdb set /examples/csv/#1/album 'You Fail Me'
# Retrieve the new item
kdb get /examples/csv/#1/album
#> You Fail Me

# The plugin stores the index of the last column
# in all of the parent keys.
kdb get user/examples/csv/#0
#> #1
kdb get user/examples/csv/#1
#> #1
kdb get user/examples/csv/#2
#> #1
kdb get user/examples/csv/#3
#> #1

# The configuration file reflects the changes
kdb file /examples/csv | xargs cat
#> album,band
#> You Fail Me,Converge
#> Pale Horses,mewithoutYou
#> Everybody Down,Kate Tempest

# Undo changes to the key database
kdb rm -r /examples/csv
sudo kdb umount /examples/csv
```

# Directory Values

By default the `csvstorage` plugin saves the name of the last column in each parent key. If you want to store a different value, you can do
so using the [Directory Value](../directoryvalue/) plugin.

```sh
# Mount plugin together with `directoryvalue` to cascading namespace `/examples/csv`
kdb mount config.csv /examples/csv csvstorage directoryvalue

# Add some data
printf 'Schindler’s List,1993,8.9\n'       >> `kdb file /examples/csv`
printf 'Léon: The Professional,1994,8.5\n' >> `kdb file /examples/csv`

# Retrieve data
kdb ls /examples/csv
#> user/examples/csv/#0
#> user/examples/csv/#0/#0
#> user/examples/csv/#0/#1
#> user/examples/csv/#0/#2
#> user/examples/csv/#1
#> user/examples/csv/#1/#0
#> user/examples/csv/#1/#1
#> user/examples/csv/#1/#2

kdb get user/examples/csv/#0/#0
#> Schindler’s List
kdb get user/examples/csv/#1/#2
#> 8.5

# The plugin stores the index of the last column in the parent keys
kdb get user/examples/csv/#0
#> #2
kdb get user/examples/csv/#1
#> #2

# Since we use the Directory Value plugin we can also change the data in a parent key
kdb set user/examples/csv/#0 'Movie – Year – Rating'
kdb set user/examples/csv/#1 'It’s a Me.'

# Retrieve data stored in parent keys
kdb get user/examples/csv/#0
#> Movie – Year – Rating
kdb get user/examples/csv/#1
#> It’s a Me.

# Undo changes to the key database
kdb rm -r /examples/csv
sudo kdb umount /examples/csv
```

#Column as index

```
kdb mount config.csv /examples/csv csvstorage "delimiter=;,header=colname,columns/index=IMDB"

printf 'IMDB;Title;Year\n'                          >> `kdb file /examples/csv`
printf 'tt0108052;Schindler´s List;1993\n'          >> `kdb file /examples/csv`
printf 'tt0110413;Léon: The Professional;1994\n'    >> `kdb file /examples/csv`

kdb ls /examples/csv
#> user/examples/csv/tt0108052
#> user/examples/csv/tt0108052/IMDB
#> user/examples/csv/tt0108052/Title
#> user/examples/csv/tt0108052/Year
#> user/examples/csv/tt0110413
#> user/examples/csv/tt0110413/IMDB
#> user/examples/csv/tt0110413/Title
#> user/examples/csv/tt0110413/Year

kdb get /examples/csv/tt0108052/Title
#> Schindler´s List

kdb rm -r /examples/csv
sudo kdb umount /examples/csv

```

## Limitations

- Does not work on file streams (e.g. `kdb import` without file)
