# elektra-hierarchy(7) -- standard hierarchy

# Integrated Mount Points

These mount points are always available.

## system:/elektra/modules

Information about currently loaded modules.

## system:/elektra/mountpoints

The mount points present in the system.

## system:/elektra/version

Version information.

# Info Mountpoints

Use `kdb mount-info` to mount these mount points.

## system:/info/elektra/constants

Gives information about how Elektra was build.

## system:/info/elektra/uname

System Information given with `uname`.

## system:/info/elektra/desktop

System Information about currently running desktop.

## system:/info/elektra/metadata

Gives information about which metadata is currently
understood by Elektra.

`METADATA.ini` needs to be mounted there.

## system:/info/elektra/contract

Gives information about clauses in plugin's contract
that is currently understood.

`CONTRACT.ini` needs to be mounted there.

## SEE ALSO

- [see namespaces tutorial](/doc/tutorials/namespaces.md)
- [elektra-namespaces(7)](elektra-namespaces.md)
