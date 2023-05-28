- infos = Information about the backendOdbc plugin is in the keys below
- infos/author = Florian Lindner <florian.lindner@student.tuwien.ac.at>
- infos/licence = BSD
- infos/provides = backend
- infos/needs =
- infos/recommends =
- infos/placements = backend
- infos/status = unfinished experimental concept nodoc
- infos/description = Plugin implementing full backend functionality for ODBC data sources

## Introduction

This plugin is a backend plugin that stores and retrieves data using ODBC (Open Database Connectivity) sources.
It was tested with unixODBC on Linux, but should also work with iODBC and Microsoft ODBC (on Windows).

The minimum requirements are a table with at least two columns:

- Key names
- Key values (strings)

The table may also contain other columns, but they are not processed by this plugin.

> If you want to use it with one of the latter two ODBC implementations, feel free to update this documentation with your experiences!

The following properties are available:

- DataSourceName: The name of the ODBC data source (as in the ODBC driver manager)
- UserName: Name of the user that should be used to connect to the data source
- Password: Password for the user who wants to connect
- TableName: Name of the table where the data is stored (default: "elektraKeys")
- KeyNameColumnName: The name of the column where the keynames are stored (by default the first column in the table is used)
- KeyStringColumnName: The name of the column where the key-values (strings) are stored (by default the second column in the table is used)

<!-- TODO [new_backend]: finish README -->
