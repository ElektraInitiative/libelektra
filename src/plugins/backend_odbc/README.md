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

The minimum requirement is a table with at least two columns:

- Key names (string, primary key)
- Key values (strings)

If you want to support metadata, a second table with at least three columns is required.

- Key name (string, FK to the first table)
- Metakey name (string)
- Metakey value (string)

The tables may also contain other columns, but they are not processed by this plugin.

The following properties are available:

- DataSourceName: The name of the ODBC data source (as in the ODBC driver manager)
- UserName: Name of the user that should be used to connect to the data source
- Password: Password for the user who wants to connect
- TableName: Name of the table where the data is stored (default: "elektraKeys")
- KeyNameColumnName: The name of the column where the keynames are stored (by default the first column in the table is used)
- KeyStringColumnName: The name of the column where the key-values (strings) are stored (by default the second column in the table is used)
- MetaTableName: The name of the table where the metadata is stored (default: metaKeys)
- MetaTableKeyColName: The name of the column in the MetaTable where the FK to the table with the keys is stored.
- MetaTableMetaKeyColName: The name of the column where the name of the metakeys is stored
- MetaTableMetaValColName: The name of the column where the value of the metakeys is stored

<!-- TODO [new_backend]: finish README -->
