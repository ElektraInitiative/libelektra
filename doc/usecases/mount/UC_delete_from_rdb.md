# Use Case: Delete configuration data from a relational database

## Summary

- **Scope:** Mounting
- **Level:** Sea
- **Actors:** Administrators
- **Brief:** The administrators can use Elektra to delete a key and its associated value stored in a supported relational database.
- **Status:** Draft

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their system.
  - The database is mounted into the KDB. (see [Use Case: Mount a data source](./UC_mount.md))

- **Main success scenario:**

  - The administrators query a value from the mounted database by giving the unique keyname. (see [Use Case: Read a configuration value from a file](./UC_read_rdb.md))
  - Elektra returns the current value of the given key to the administrators.
  - The administrators command Elektra to remove the key and its associated value by giving the unique keyname.
  - Elektra removes the affected tuple from the database.
  - The administrators query the same key as in the first step from Elektra.
  - Elektra gives an error message to the administrators which tells them that the requested key does not exist.
  - The administrators query the database directly to verify that the requested key and value were deleted.

- **Alternative scenario:**

  - The requested key is not found in the database.
    - Elektra reports the error to the administrators.

- **Error scenario:**

  - The connection to the database failed. (e.g. network error)
    - Elektra reports the error to the administrators.
  - Writing to the database table(s) failed. (e.g. because of missing permissions or full storage)
    - Elektra reports the error to the administrators.

- **Postconditions:**

  - If the administrators use Elektra to read the value of the removed key, an error message must be returned.
  - The change must be written to the mounted database. (can be verified by querying the database directly, e.g. by using SQL)

- **Non-functional Constraints:**

  - Deleting a key stored in the mounted database through Elektra should not take more than twice the time that is required to delete the tuple directly in the database without using Elektra. (e.g. by using SQL)
    - For access times less than one second, the access through Elektra should not add more than one second.
    - If the datasource is accessed through an intermediate data access layer (e.g. ODBC), than the same data access layer is also used for retrieving the time for the access without Elektra.
