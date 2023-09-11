# Use Case: Unmount a data source

## Summary

- **Scope:** Mounting
- **Level:** Kite
- **Actors:** Administrators
- **Brief:** The administrators can detach previously mounted data sources from the KDB of Elektra.
- **Status:** Draft (currently only files are supported)

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their system.
  - The data source was successfully mounted before. (see [Use Case: Mount a data source](./UC_mount.md))

- **Main success scenario:**

  - The administrators list all current mountpoints. (see [Use Case: List all mountpoints](./UC_list_mountpoints.md))
    - The data source to unmount must be a part of the list.
  - The administrators specify the mountpoint of the data source they want to unmount.
  - The given mountpoint is removed and the data source gets detached from Elektra.
  - The administrators list all current mountpoints. (see [Use Case: List all mountpoints](./UC_list_mountpoints.md))
    - The data source to unmount must **NOT** be a part of the list.

- **Alternative scenario:**

  - The given mountpoint is not found.
    - Elektra reports the errors to the administrators.

- **Error scenario:**

  - The opening of the data source failed. (e.g. the mounted file is no longer available)
    - Elektra reports the errors to the administrators.
  - The unmounting is not possible. (e.g. because the data source is currently in use by Elektra)
    - Elektra reports the errors to the administrators.

- **Postconditions:**

  - The specified configuration data source can no longer be accessed through the KDB of Elektra.
  - Queries to the KDB do no longer include results from the unmounted data source.

- **Non-functional Constraints:**

  - The unmounting of a data sources should not take longer than 5 seconds (reference system has to be defined).
    - Delays caused by slow storages or network connections are not a part of that delay, because Elektra is not responsible for that areas.
