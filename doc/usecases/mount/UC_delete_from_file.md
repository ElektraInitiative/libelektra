# Use Case: Delete configuration data from a file

## Summary

- **Scope:** Mounting
- **Level:** Sea
- **Actors:** Administrators
- **Brief:** The administrators can use Elektra to delete a key and its associated value if it is stored in a file that has a supported format.
- **Status:** Implemented

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their system.
  - The configuration file is mounted into the KDB. (see [Use Case: Mount a data source](./UC_mount.md))

- **Main success scenario:**

  - The administrators query a value from the mounted file by giving the unique keyname. (see [Use Case: Read a configuration value from a file](./UC_read_file.md))
  - Elektra returns the current value of the given key to the administrators.
  - The administrators command Elektra to remove the key and its associated value by giving the unique keyname.
  - Elektra removes the entry for the given key from the mounted configuration file.
  - The administrators query the same keyname as in the first step.
  - Elektra now gives a message to the administrators that tells them that the requested key does not exist.
  - The administrators open the configuration file in a text editor to check if the entry was correctly deleted from the file. (applies only to text-based file formats)

- **Alternative scenario:**

  - The requested key is not found in the configuration file.
    - Elektra reports the error to the administrators.

- **Error scenario:**

  - The opening of the configuration file failed. (e.g. broken storage, faulty filesystem, network error).
    - Elektra reports the error to the administrators.
  - Writing to the configuration file failed. (e.g. because of missing permissions)
    - Elektra reports the error to the administrators.

- **Postconditions:**

  - If the administrators use Elektra to read the value of the removed key, an error message must be returned.
  - The change must be written to the mounted configuration file. (can be verified by opening the file with a text editor)

- **Non-functional Constraints:**

  - Deleting a key stored in the mounted file through Elektra should not take more than twice the time that is required to
    edit the file with a text editor and do the same change.
    - Only the loading and saving of the file is counted, not the time the administrators need to edit the file.
    - For a benchmark, the editing of the file should be scripted.
    - For access times less than one second, the access through Elektra should not add more than one second.
    - This constraint is only applicable for text-based file formats, not for binary files.
