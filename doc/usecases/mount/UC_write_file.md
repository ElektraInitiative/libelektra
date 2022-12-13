# Use Case: Write new configuration data to a file

## Summary

- **Scope:** Mounting
- **Level:** Sea
- **Actors:** Administrators
- **Brief:** The administrators can use Elektra to add a new configuration data entry (key and its associated value) to a file that has a supported format.
- **Status:** Implemented

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their system.
  - The configuration file is mounted into the KDB. (see [Use Case: Mount a data source](./UC_mount.md))

- **Main success scenario:**

  - The administrators use Elektra to create a new key that belongs to the mountpoint of the file where the key and value should be stored.
  - Elektra stores the new configuration data entry (key and associated value) in the mounted configuration file.
  - The administrators query the new keyname using Elektra and get the value they specified before.
  - The administrators open the mounted file with a text editor to verify that the data was stored in the correct place. (applies only to text-based file formats)

- **Alternative scenario:**

  - An invalid keyname was specified.
    - Elektra reports the error to the administrators.

- **Error scenario:**

  - The opening of the configuration file failed. (e.g. broken storage, faulty filesystem, network error)
    - Elektra reports the error to the administrators.
  - Writing to the configuration file failed. (e.g. because of missing permissions)
    - Elektra reports the error to the administrators.

- **Postconditions:**

  - If the administrators use Elektra to read the value of the new key, its value must be returned.
  - The change must be written to the mounted configuration file. (can be verified by opening the file with a text editor)

- **Non-functional Constraints:**

  - Writing a new entry to the mounted file through Elektra should not take more than twice the time that is required to
    edit the file with a text editor and add the same entry.
    - Only the loading and saving of the file is counted, not the time the administrators need to edit the file.
    - For a benchmark, the editing of the file should be scripted.
    - For access times less than one second, the access through Elektra should not add more than one second.
    - This constraint is only applicable for text-based file formats, not for binary files.
