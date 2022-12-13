# Use Case: List all configuration keys and values from all currently mounted data sources

## Summary

- **Scope:** Mounting
- **Level:** Kite
- **Actors:** Administrators
- **Brief:** The administrators can use Elektra to get a list of all keynames and associated values from all currently mounted data sources.
- **Status:** Implemented (currently only file-based data sources can be mounted)

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their systems.

- **Main success scenario:**

  - The administrators query a list with all keys and values from the currently mounted data sources.
  - Elektra returns the requested list.

- **Alternative scenario:**

  - none

- **Error scenario:**

  - The reading from at least one mounted data source failed.
    - Elektra reports the error to the administrators.

- **Postconditions:**

  - The administrators can see a list with all keynames and values from all mounted data sources.
  - Nothing is written to any of the data sources.

- **Non-functional Constraints:**

  - Creating the list should not take longer than twice the time to query all mounted data source without Elektra.
    - This means for text-based files to print them to stdout and for databases to select all entries (`SELECT * from <table>`)
    - For access times less than one second (sum of access times for all mounted data sources), the access through Elektra should not add more than one second.
