- infos = Information about hosts plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/hosts
- infos/needs =
- infos/recommends = glob error network
- infos/placements = getstorage setstorage
- infos/status = maintained unittest nodep libc limited
- infos/metadata = order comment/# comment/#/start comment/#/space
- infos/description = This plugin reads and writes /etc/hosts files.

## Introduction

The `/etc/hosts` file is a simple text file that associates IP addresses
with hostnames, one line per IP address. The format is described in `hosts(5)`.

## Special values

### Hostnames

Canonical hostnames are stored as key names with their IP address
as value.

### Aliases

Aliases are stored as sub keys with a read only duplicate of the
associated IP address as value.

### Comments

Comments are stored according to the comment metadata specification
(see [/doc/METADATA.ini](/doc/METADATA.ini) for more information).

### Ordering

The ordering of the hosts is stored in metakeys of type `order`.
The value is an ascending number. Ordering of aliases is
_not_ preserved.

## Examples

Mount the plugin:

```bash
sudo kdb mount --with-recommends /etc/hosts system/hosts hosts
```

Print out all known hosts and their aliases:

```bash
kdb ls system/hosts
```

Get IP address of ipv4 host "localhost":

```bash
kdb get system/hosts/ipv4/localhost
```

Check if a comment is belonging to host "localhost":

```bash
kdb meta-ls system/hosts/ipv4/localhost
```

Try to change the host "localhost", should fail because it is not an
IPv4 address:

```bash
sudo kdb set system/hosts/ipv4/localhost ::1
```

```sh
# Backup-and-Restore:/tests/hosts

sudo kdb mount --with-recommends hosts /tests/hosts hosts

# Create hosts file for testing
echo '127.0.0.1	localhost' >  `kdb file /tests/hosts`
echo '::1	localhost'	    >> `kdb file /tests/hosts`

# Check the file
cat `kdb file /tests/hosts`
#> 127.0.0.1	localhost
#> ::1	localhost

# Check if the values are read correctly
kdb get /tests/hosts/ipv4/localhost
#> 127.0.0.1
kdb get /tests/hosts/ipv6/localhost
#> ::1

# Should both fail with error 04200 and return 5
kdb set /tests/hosts/ipv4/localhost ::1
# RET:5
# ERROR:C03200
kdb set /tests/hosts/ipv6/localhost 127.0.0.1
# RET:5
# ERROR:C03200

# cleanup
kdb rm -r /tests/hosts
sudo kdb umount /tests/hosts
```
