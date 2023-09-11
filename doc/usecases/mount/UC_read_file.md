# Use Case: Read a configuration value from a file

## Summary

- **Scope:** Mounting
- **Level:** Sea
- **Actors:** Administrators
- **Brief:** The administrators can use Elektra to read key-value based configuration data from a configuration file that has a supported format.
- **Status:** Implemented

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their systems.
  - The configuration file is mounted into the KDB. (see [Use Case: Mount a data source](./UC_mount.md))

- **Main success scenario:**

  - The administrators query a value from the mounted configuration file by giving the unique keyname.
  - Elektra reads the value from the mounted configuration file.
  - Elektra returns the requested value to the administrators.

- **Alternative scenario:**

  - The requested key is not found in the configuration file.
    - Elektra reports the error to the administrators.

- **Error scenario:**

  - The opening of the configuration file failed. (e.g. broken storage, faulty filesystem, network error).
    - Elektra reports the error to the administrators.

- **Postconditions:**

  - The administrators can see the value for the requested keyname.
  - The mounted file is not changed.

- **Non-functional Constraints:**

  - Reading the value of a key stored in the mounted file through Elektra should not take more than twice the time that is required to open the file in a text editor.
    - For access times less than one second, the access through Elektra should not add more than one second.
    - This constraint is only applicable for text-based file formats, not for binary files.
