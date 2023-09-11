# Use Case: Read a configuration value from a relational database

## Summary

- **Scope:** Mounting
- **Level:** Sea
- **Actors:** Administrators
- **Brief:** The administrators can use Elektra to read key-value based configuration data from a supported relational database.
- **Status:** Draft

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their system.
  - The database is mounted into the KDB. (see [Use Case: Mount a data source](./UC_mount.md))

- **Main success scenario:**

  - The administrators query a value from the mounted database by giving the unique keyname to Elektra.
  - Elektra reads the value from the mounted database.
  - The requested value for the given keyname is returned to the administrators.

- **Alternative scenario:**

  - The requested key is not found in the database.
    - Elektra reports the error to the administrators.

- **Error scenario:**

  - The connection to the database failed. (e.g. network error)
    - Elektra reports the error to the administrators.

- **Postconditions:**

  - The administrators can see the values that are stored in the database by using a tool that queries Elektras KDB.
  - The content of the database is not changed.

- **Non-functional Constraints:**

  - Reading the value of a key stored in the mounted database through Elektra should not take more than twice the time that is required to query the value directly from the database without using Elektra. (e.g. by using SQL)
    - For access times less than one second, the access through Elektra should not add more than one second.
    - If the datasource is accessed through an intermediate data access layer (e.g. ODBC), than the same data access layer is also used for retrieving the time for the access without Elektra.
