- infos = Information about the backendOdbc plugin is in the keys below
- infos/author = Florian Lindner <florian.lindner@student.tuwien.ac.at>
- infos/licence = BSD
- infos/provides = backend
- infos/needs =
- infos/recommends =
- infos/placements = backend
- infos/status = experimental reviewed
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

The tables may also contain other columns, but they are not processed by this plugin and must support NULL- or DEFAULT-values, if you want to add tuples to the table via Elektra (e.g. by calling `kdb set <key> <value>`).

If the column for the key-name is not defined as a primary key and multiple rows contain the same key-name, they are treated as one key, where the value is taken from the first row and the metadata is combined.
If multiple metakeys with the same metakey-name exist for a key, the last metavalue is used.
This behavior is part of the internal algorithm of the plugin and could change in the future.
So if you want a predictable and expectable behavior, make sure that the described primary- and foreign-key constraints are respected.

## Mounting

The [mountpoint definition](/doc/dev/mountpoints.md) for ODBC mountpoints, which is stored at `system:/elektra/mountpoints/<MP>/definition`
is defined as follows. The placeholder `<MP>` is the path in the KDB where the root of the defined mountpoint is located.
This is the path you specify when creating the mountpoint.

The key-names for an ODBC mountpoint-definition are:

```
system:/elektra/mountpoints/<mp>
system:/elektra/mountpoints/<mp>/plugins/backend
system:/elektra/mountpoints/<mp>/plugins/backend/name
system:/elektra/mountpoints/<mp>/definition/dataSourceName
system:/elektra/mountpoints/<mp>/definition/userName
system:/elektra/mountpoints/<mp>/definition/password
system:/elektra/mountpoints/<mp>/definition/timeout
system:/elektra/mountpoints/<mp>/definition/table/name
system:/elektra/mountpoints/<mp>/definition/table/keyColName
system:/elektra/mountpoints/<mp>/definition/table/valColName
system:/elektra/mountpoints/<mp>/definition/metaTable/name
system:/elektra/mountpoints/<mp>/definition/metaTable/keyColName
system:/elektra/mountpoints/<mp>/definition/metaTable/metaKeyColName
system:/elektra/mountpoints/<mp>/definition/metaTable/metaValColName
```

The key-values for the keys under `definition` are defined as follows:

- _dataSourceName:_ The name of the ODBC data source (as defined in the odbc.ini file)
- _userName:_ Name of the user that should be used to connect to the data source
  - can be empty if no username is required or if the value should be read from the odbc.ini file
- _password:_ Password for the user who wants to connect
  - can be empty if no password is required or if the value should be read from the odbc.ini file
- _timeout:_ The number of seconds to wait after a connection attempt is considered as failed (0 = wait indefinitely)
  - if empty, a default value is used
- _table/name:_ Name of the table where the data is stored
- _table/keyColName:_ The name of the column where the keynames are stored
- _table/valColName:_ The name of the column where the key-values (strings) are stored
- _metaTable/name:_ The name of the table where the metadata is stored
- _metaTable/keyColName:_ The name of the column in the MetaTable where the name of the key the metakey belongs to is stored
- _metaTable/metaKeyColName:_ The name of the column where the name of the metakeys is stored
- _metaTable/metaValColName:_ The name of the column where the value (string) of the metakeys is stored

There is a new command for the kdb-tool: `kdb mountOdbc`.
Please be aware that the ODBC backend, in contrast to the classic file-based backend,
currently does not support adding other plugins to the mountpoint.

The `kdb mountOdbc` command accepts arguments for the previously described values of the _mountpoint definition_.
For further details, please refer to the [man page](/doc/help/kdb-mountOdbc.md) for that command.

For unmounting, there is no new command necessary.
Just use the well-known `kdb umount <mountpoint>`, exactly like for the file-based backend.

For more information about the ODBC backend, there is a tutorial available at [/doc/tutorials/odbc-backend.md](/doc/tutorials/odbc-backend.md).
