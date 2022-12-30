- infos = Storage Plugin for the LDIF format
- infos/author = Burkhard Hampl <e11776165@student.tuwien.ac.at>, Richard Stöckl <e11908080@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage/ldif
- infos/recommends =
- infos/placements = getstorage setstorage
- infos/status = libc configurable experimental
- infos/metadata =
- infos/description = deserialize LDIF files

## Introduction

This is the Elektra LDIF storage plugin whose aim is to provide support for reading and writing LDIF files.

Each LDIF entry gets mapped into the Elektra tree in the following way:

- the `dn` gets mapped to the key, so `uid=willi,dc=example,dc=org` will be mapped to the Elektra key `$MOUNTPOINT/dc=org/dc=example/uid=willi`
- every LDIF attribute will use this base key to store all attributes in

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Dependencies

OpenLDAP

## Examples

```sh
# Backup-and-Restore: user:/tests/ldif

echo "$PWD/src/plugins/ldif/ldif/simple-people.ldif"
# Mount the provided example: simple-people.ldif

kdb mount "$PWD/src/plugins/ldif/ldif/simple-people.ldif" /tests/people ldif

kdb get system:/tests/people/dc=org/dc=libelektra/ou=developer/uid=heidi/dn
#> uid=heidi,ou=developer,dc=libelektra,dc=org

kdb get system:/tests/people/dc=org/dc=libelektra/ou=developer/uid=heidi/uid
#> heidi

kdb get system:/tests/people/dc=org/dc=libelektra/ou=developer/uid=heidi/cn
#> Heidi Redlbacher
```
