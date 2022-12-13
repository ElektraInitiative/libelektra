# Use Case: Mount a data source

## Summary

- **Scope:** Mounting
- **Level:** Kite
- **Actors:** Administrators
- **Brief:** The administrators can include configuration data from different data sources into Elektra.
- **Status:** Draft (currently only files are supported)

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their system.

- **Main success scenario:**

  - The administrators specify a mountpoint in the KDB and an identifier for the data source they want to mount.
    - For file-based data sources, the identifier is usually the path and filename.
  - The data source is successfully mounted into the KDB.

- **Alternative scenario:**

  - The given data source is not found. (e.g. wrong filename)
    - Elektra reports the error to the administrators.
  - The given data source is not supported by Elektra. (e.g. wrong file format, syntax errors)
    - Elektra reports the error to the administrators.

- **Error scenario:**

  - The opening of the data source failed.
    - Elektra reports the error to the administrators.

- **Postconditions:**

  - The newly mounted data source is listed when querying the current mountpoints. (see [Use Case: List all mountpoints](./UC_list_mountpoints.md))
  - The administrators can now work (CRUD) with the configuration data stored in the newly mounted data source using Elektras key-value-based interface. (see other mounting use cases)

- **Non-functional Constraints:**

  - The mounting of new data sources should not take longer than 5 seconds (reference system has to be defined).
    - Delays caused by slow storages or network connections are not a part of that constraint, because Elektra is not responsible for that areas.
