# Use Case: Write new configuration data to a relational database

## Summary

- **Scope:** Mounting
- **Level:** Sea
- **Actors:** Administrators
- **Brief:** The administrators can use Elektra to add a new configuration data entry (key and its associated value) to a supported relational database.
- **Status:** Draft

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their system.
  - The database is mounted into the KDB. (see [Use Case: Mount a data source](./UC_mount.md))

- **Main success scenario:**

  - The administrators use Elektra to create a new key that belongs to the mountpoint of the database where the key and value should be stored.
  - Elektra stores the new configuration data entry (key and associated value) in the mounted database.
  - The administrators query the new keyname from Elektra and get the value they specified before.
  - The administrators query the database directly (e.g. by using SQL) to verify that the data was stored in the correct place.

- **Alternative scenario:**

  - An invalid keyname was specified.
    - Elektra reports the error to the administrators.

- **Error scenario:**

  - The query to the database failed. (e.g. network error)
    - Elektra reports the error to the administrators.
  - Writing to the database failed. (e.g. missing permissions or full storage)
    - Elektra reports the error to the administrators.

- **Postconditions:**

  - If the administrators use Elektra to read the value of the new key, its value must be returned.
  - The change must be written to the mounted database.

- **Non-functional Constraints:**

  - Adding a new key and associated value to the mounted database through Elektra should not take more than twice the time that is required to add the data directly to the database without using Elektra. (e.g. by using SQL)
    - For access times less than one second, the access through Elektra should not add more than one second.
    - If the datasource is accessed through an intermediate data access layer (e.g. ODBC), than the same data access layer is also used for retrieving the time for the access without Elektra.
