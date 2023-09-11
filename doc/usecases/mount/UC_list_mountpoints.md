# Use Case: List all mountpoints

## Summary

- **Scope:** Mounting
- **Level:** Kite
- **Actors:** Administrators
- **Brief:** The administrators can query the current mountpoints.
- **Status:** Draft (currently only file-based data sources can be listed)

## Scenarios

- **Precondition:**

  - The administrators have a working installation of Elektra and a tool to access the KDB (key database) on their systems.

- **Main success scenario:**

  - The administrators use Elektra to query a list with all current mountpoints.
  - Elektra returns the list with the mountpoints and the data source identifiers. (e.g. filename and path, data source name)
  - The administrators mount a new data source. (see [Use Case: Mount a data source](./UC_mount.md))
  - The administrators use Elektra to query a list with all current mountpoints.
  - The new mountpoint is now added to the list.
  - The administrators unmount the previously mounted data source. (see [Use Case: Unmount a data source](./UC_unmount.md))
  - The administrators use Elektra to query a list with all current mountpoints.
  - The mountpoint for the unmounted data source is now not included in the list.

- **Alternative scenario:**

  - none new
  - The scenarios of the used use cases (mounting and unmounting) apply.

- **Error scenario:**


- **Postconditions:**

  - The administrators get a (possibly empty) list of all data sources that are currently mounted into the KDB.
  - No data in any of the mounted data sources has changed.

- **Non-functional Constraints:**

  - Listing the mountpoints should not take more than three seconds. (reference system has to be defined) for less than 100 mountpoints
