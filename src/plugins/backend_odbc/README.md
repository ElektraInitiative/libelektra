- infos = Information about the backendOdbc plugin is in the keys below
- infos/author = Florian Lindner <florian.lindner@student.tuwien.ac.at>
- infos/licence = BSD
- infos/provides = backend
- infos/needs =
- infos/recommends =
- infos/placements = backend
- infos/status = unfinished experimental
- infos/description = Plugin implementing full backend functionality for ODBC data sources

## Introduction

This plugin is a backend plugin that stores and retrieves data using ODBC (Open Database Connectivity) data sources.
It was tested with unixODBC on Linux, but should also work with iODBC and Microsoft ODBC (on Windows).

> If you want to use it with one of the latter two ODBC implementations, feel free to update this documentation with your experiences!

## Required database scheme

The minimum requirement is a table with at least two columns:

- Key-name (string, primary key (PK))
- Key-value (string)

Additionally, a second table with at least three columns is required.

- Key-name (string, foreign key (FK) to the first table)
- Metakey-name (string)
- Metakey-value (string)

The primary key of this table consists of two columns: the **key-name** and the **metakey-name**.
In the language of ER-modelling, the metatable can therefore by considered a **weak-entity**.

> Currently, only data sources with tables for metadata are supported!
> So you have to define a meta-table.
> Data sources without a table for metadata will probably be supported in the future.
> If you want to use metadata, the ODBC driver for your data source has to support **outer joins**.
> This implies that currently, only ODBC drivers with support for outer joins are supported by the ODBC backend.

The tables may also contain other columns, but they are not processed by this plugin and must support NULL- or DEFAULT-values,
if you want to add tuples to the table via Elektra (e.g. by calling `kdb set <key> <value>`).

The following properties are available:

- DataSourceName: The name of the ODBC data source (as in the ODBC driver manager)
- UserName: Name of the user that should be used to connect to the data source
  - pass NULL if no username is required or if the value should be read from the odbc.ini file
- Password: Password for the user who wants to connect
  - pass NULL if no password is required or if the value should be read from the odbc.ini file
- TableName: Name of the table where the data is stored
- KeyNameColumnName: The name of the column where the keynames are stored
- KeyStringColumnName: The name of the column where the key-values (strings) are stored
- MetaTableName: The name of the table where the metadata is stored
- MetaTableKeyColName: The name of the column in the MetaTable where the FK to the table with the keys is stored
- MetaTableMetaKeyColName: The name of the column where the name of the metakeys is stored
- MetaTableMetaValColName: The name of the column where the value of the metakeys is stored

## Mounting

There is a new command for the kdb-tool: `kdb mountOdbc`.
Please be aware that this command, in contrast to the classic `kdb mount` for the file-backend,
currently does not support adding other plugins to the mountpoint.
This should change when the new tooling and mounting library are finished.

For unmounting, there is no new command necessary.
Just use the well-knows `kdb umount <mountpoint>`, exactly like for the file-based backend.

For more information about the ODBC backend, there is a tutorial available at
[/doc/tutorials/odbc-backend.md](/doc/tutorials/odbc-backend.md).
