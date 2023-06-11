# kdb-mountOdbc(1) - Mount an ODBC data source to the key database

## SYNOPSIS

`kdb mountOdbc <data source name> <user name> <password> <table name> <key column name> <value column name> <meta table name>
<mt key column name> <mt metakey column name> <mt metavalue column name> <timeout (s)> <mountpoint>`<br>

- Where `data source name` is the name of the ODBC data source as defined in `odbc.ini` (usually stored at `/etc/unixODBC/`).
- `user name`: The username that should be used for connecting to the ODBC data source. If no username is needed, or it is already specified in the ini-file for the configuration of the datasource, `""` can be provided for this argument.
- `password`: The same rules as for `user name` also apply to this argument.
- `table name`: The name of the table in the data source where the keys and their values should be stored.
- `key column name`: The name of the column where the key-names should be stored. This should be the primary key of the table.
- `value column name`: The name of the column where the key-values (strings) should be stored. This column should support NULL-values.
- `meta table name`: The name of the table where the metadata for the keys should be stored.
- `mt key column name`: The name of the column in the meta table where the key-name should be stored. This should be a foreign key that refers to the column with the key-name of the other table.
- `mt metakey column name`: The name of the column in the meta table where the name of the metakey should be stored. This column, together with the column for the key-name, should define the primary key of the meta table.
- `mt metavalue column name`: The name of the column in the meta table where the value (string) of the metakey should be stored. This column should support NULL-values.
- `timeout (s)`: The timeout (in seconds) that should be used when connecting to the data source. When passing '0', the timeout gets disabled and the application could potentially wait forever. So use this option with care! If you want to use a default timeout, you can just pass `""` for this argument.
- `mountpoint`: The place in the KDB where the ODBC data source should be mounted. The syntax is the same as with the file-based backend, but you can only use `user:/` and `system:/` namespaces as mountpoints for ODBC data sources.

> All columns in the data source that are specified via one of the described arguments, should be of type `TEXT`.
> The tables can have additional columns. These are not processed by the ODBC backend, but should support NULL or DEFAULT values.

> Only ODBC drivers that support **outer joins** are suited for the Elektra ODBC backend.

## DESCRIPTION

This command allows a user to persistently mount a new _backend_ that refers to an ODBC data source.
The concept is the same as for mounting with the file-based backend.
So it is recommended to also read to man page for [kdb-mount(1)](kdb-mount.md).
More about mounting is explained in [elektra-mounting(7)](elektra-mounting.md).

## IMPORTANT

This command writes into the `/etc` directory to make the mounting persistent.
As such it requires root permissions.
Use `kdb file system:/elektra/mountpoints` to find out where exactly it will write to.

> There is no special command for unmounting ODBC data sources.
> You can just use the [kdb-umount(1)](kdb-umount.md) command the same way as with mountpoints for files.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional information in case of errors/warnings.
- `-f`, `--force`:
  Create the mountpoint even if the data source can not be reached at mount-time.

## Examples

To mount the sample SQLite data source:<br>
`kdb mountOdbc Selektra "" "" elektraKeys keyName keyValue metaKeys keyName metaKeyName metaKeyValue "" user:/odbcSqlite`

To specify all arguments:<br>
`kdb mountOdbc Selektra myUser myPassword elektraKeys keyName keyValue metaKeys keyName metaKeyName metaKeyValue 12 user:/odbcSqlite`

## SEE ALSO

- [kdb-mount(1)](kdb-mount.md).
- [elektra-glossary(7)](elektra-glossary.md).
- [kdb-spec-mount(1)](kdb-spec-mount.md).
- [kdb-umount(1)](kdb-umount.md).
- [elektra-mounting(7)](elektra-mounting.md).
