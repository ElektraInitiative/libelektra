- infos = Information about the csvstorage plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/csv
- infos/needs =
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = unittest nodep libc configurable
- infos/features/storage = limited
- infos/description = parses CSV files

## Introduction

This plugin allows you to read and write CSV files using Elektra.
It aims to be compatible with RFC 4180.
Rows and columns are written using Elektra's arrays (`#0`, `#1`,..).
By configuring the plugin you can give columns a name.

## Configuration

`delimiter`
Tells the plugin what delimiter is used in the file.
The default delimiter is `,` and will be used if `delimiter` is not set.

`header`
Tells the plugin to use the first line as a header if it is set to `colname`.
The columns will get the corresponding names.
Skip the first line if it is set to `skip` or treat the first line as a record if it is set to `record`.
If `header` is not set, or set to `record`, the columns get named #0,#1,... (array key naming)

`columns`
If this key is set the plugin will yield an error for every file that doesn't have exactly the amount of columns as specified in `columns`.

`columns/names`
Sets the column names. Only usable in combination with the `columns` key. The number of subkeys must match the number of columns.
Conflicts with usage of `header`.

`columns/index`
Specifies which column should be used to index records instead of the record number.

`export=,export/<column name>=`
Export column `column name`:

- The key `export` must be present, additionally to `export/<column name>`
- Also multiple column names can be given for different columns to export.
  - Then `delimiter` will be used as delimiter (`,` as default).
  - The order depends on the alphabetic order of the column names.
    Use `awk -F',' 'BEGIN{OFS=","} {print $2, $1, $3}'` or similar to reorder.
  - Unknown column names are ignored.

## Examples

First line should determine the headers:

```bash
kdb mount test.csv /csv csvstorage \
  "delimiter=;,header=colname,columns=2,columns/names,columns/names/#0=col0Name,columns/names/#1=col1Name"
```

### Usage

The example below shows how you can use this plugin to read and write CSV files.

```sh
# Mount plugin to `user:/tests/csv`
# We use the column names from the first line of the
# config file as key names
sudo kdb mount config.csv user:/tests/csv csvstorage  "header=colname,columns/names/#0=col0Name,columns/names/#1=col1Name"

# Add some data
printf 'band,album\n'                           >> `kdb file user:/tests/csv`
printf 'Converge,All We Love We Leave Behind\n' >> `kdb file user:/tests/csv`
printf 'mewithoutYou,Pale Horses\n'             >> `kdb file user:/tests/csv`
printf 'Kate Tempest,Everybody Down\n'          >> `kdb file user:/tests/csv`

kdb ls user:/tests/csv
#> user:/tests/csv/#0
#> user:/tests/csv/#0/album
#> user:/tests/csv/#0/band
#> user:/tests/csv/#1
#> user:/tests/csv/#1/album
#> user:/tests/csv/#1/band
#> user:/tests/csv/#2
#> user:/tests/csv/#2/album
#> user:/tests/csv/#2/band
#> user:/tests/csv/#3
#> user:/tests/csv/#3/album
#> user:/tests/csv/#3/band

# The first array element contains the column names
kdb get user:/tests/csv/#0/band
#> band
kdb get user:/tests/csv/#0/album
#> album

# Retrieve data from the last entry
kdb get user:/tests/csv/#3/album
#> Everybody Down
kdb get user:/tests/csv/#3/band
#> Kate Tempest

# Change an existing item
kdb set user:/tests/csv/#1/album 'You Fail Me'
# Retrieve the new item
kdb get user:/tests/csv/#1/album
#> You Fail Me

# The plugin stores the index of the last column
# in all of the parent keys.
kdb get user:/tests/csv/#0
#> #1
kdb get user:/tests/csv/#1
#> #1
kdb get user:/tests/csv/#2
#> #1
kdb get user:/tests/csv/#3
#> #1

# The configuration file reflects the changes
kdb file user:/tests/csv | xargs cat
#> album,band
#> You Fail Me,Converge
#> Pale Horses,mewithoutYou
#> Everybody Down,Kate Tempest

# Undo changes to the key database
kdb rm -r user:/tests/csv
sudo kdb umount user:/tests/csv
```

# Column as index

```
kdb mount config.csv /tests/csv csvstorage "delimiter=;,header=colname,columns/index=IMDB"

printf 'IMDB;Title;Year\n'                          >> `kdb file /tests/csv`
printf 'tt0108052;Schindler´s List;1993\n'          >> `kdb file /tests/csv`
printf 'tt0110413;Léon: The Professional;1994\n'    >> `kdb file /tests/csv`

kdb ls /tests/csv
#> user:/tests/csv/tt0108052
#> user:/tests/csv/tt0108052/IMDB
#> user:/tests/csv/tt0108052/Title
#> user:/tests/csv/tt0108052/Year
#> user:/tests/csv/tt0110413
#> user:/tests/csv/tt0110413/IMDB
#> user:/tests/csv/tt0110413/Title
#> user:/tests/csv/tt0110413/Year

kdb get /tests/csv/tt0108052/Title
#> Schindler´s List

kdb rm -r /tests/csv
sudo kdb umount /tests/csv

```

# Export filter

```
kdb mount config.csv /tests/csv csvstorage "delimiter=;,header=colname,columns/index=IMDB"

printf 'IMDB;Title;Year\n'                          >> `kdb file /tests/csv`
printf 'tt0108052;Schindler´s List;1993\n'          >> `kdb file /tests/csv`
printf 'tt0110413;Léon: The Professional;1994\n'    >> `kdb file /tests/csv`

kdb export /tests/csv csvstorage -c "delimiter=;,header=colname,columns/index=IMDB,export=,export/IMDB=,export/Title="
#> IMDB;Title
#> tt0108052;Schindler´s List
#> tt0110413;Léon: The Professional


kdb export /tests/csv csvstorage -c "delimiter=;,header=colname,columns/index=IMDB,export=,export/IMDB=,export/Year="
#> IMDB;Year
#> tt0108052;1993
#> tt0110413;1994


kdb rm -r /tests/csv
sudo kdb umount /tests/csv

```

## Array metakey

```sh
kdb mount config.csv user:/tests/csvstorage csvstorage

kdb set user:/tests/csvstorage/test test

printf 'one,two,three\nfour,five,six\n' > `kdb file user:/tests/csvstorage`

kdb ls user:/tests/csvstorage
#> user:/tests/csvstorage/#0
#> user:/tests/csvstorage/#0/#0
#> user:/tests/csvstorage/#0/#1
#> user:/tests/csvstorage/#0/#2
#> user:/tests/csvstorage/#1
#> user:/tests/csvstorage/#1/#0
#> user:/tests/csvstorage/#1/#1
#> user:/tests/csvstorage/#1/#2

kdb meta-get user:/tests/csvstorage/#0 array
#> #2

kdb rm -r user:/tests/csvstorage

kdb umount user:/tests/csvstorage
```

## Limitations

- Does not work on file streams (e.g. `kdb import` without file)
- When using csvstorage for exporting, all parent keys must be present
  (see https://issues.libelektra.org/2304)
