# Use cases for mounting

This folder contains the use cases for mounting, which means accessing configuration data sources through Elektra.
The use cases here cover configuration files and databases.

The goal is to have a common key-value based view of configuration data that is stored in different data sources.

## Basic use cases

- [Mount a data source](./UC_mount.md)
- [Unmount a data source](./UC_unmount.md)
- [List all mountpoints](./UC_list_mountpoints.md)
- [List all configuration keys and values from all currently mounted data sources](./UC_list_values.md)

## CRUD (Create, Read, Update, Delete) use cases

- [Read a configuration value from a file](./UC_read_file.md)
- [Edit a configuration value stored in a file](./UC_edit_file.md)
- [Write new configuration data to a file](./UC_write_file.md)
- [Delete configuration data from a file](./UC_delete_from_file.md)

- [Read a configuration value from a relational database](./UC_read_rdb.md)
- [Edit a configuration value stored in a relational database](./UC_edit_rdb.md)
- [Write new configuration data to a relational database](./UC_write_rdb.md)
- [Delete configuration data from a relational database](./UC_delete_from_rdb.md)
