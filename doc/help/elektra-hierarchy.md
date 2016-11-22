elektra-hierarchy(7) -- standard hierarchy
==========================================

# Integrated Mountpoints

These mountpoints are always available.


## system/elektra/modules

Information about currently loaded modules.


## system/elektra/mountpoints

The mountpoints present in the system.


## system/elektra/version

Version information.



# Info mountpoints

Use `kdb mount-info` to mount these mountpoints.

## system/info/constants

Gives information about how Elektra was build.


## system/info/uname

System Information given with `uname`.


## system/info/desktop

System Information about currently running desktop.


## system/info/metadata

Gives information about which metadata is currently
understood by Elektra.

`METADATA.ini` needs to be mounted there.


## system/info/contract

Gives information about clauses in plugin's contract
that is currently understood.

`CONTRACT.ini` needs to be mounted there.


## SEE ALSO

- [see namespaces tutorial](/doc/tutorials/namespaces.md)
- [elektra-namespaces(7)](elektra-namespaces.md)
