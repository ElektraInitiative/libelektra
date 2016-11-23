- infos = Information about HOSTS plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/hosts
- infos/needs =
- infos/recommends = glob error network
- infos/placements = getstorage setstorage
- infos/status = maintained unittest nodep libc limited
- infos/description = This plugin reads and writes /etc/hosts files.

## Introduction ##

The `/etc/hosts` file is a simple text file that associates IP addresses
with hostnames, one line per IP address. The format is described in `hosts(5)`.

## Special values ##

### Hostnames ###

Canonical hostnames are stored as key names with the IP address as key
value. 

### Aliases ###

Aliases are stored as sub keys with a read only duplicate of the
associated ip address as value. 

### Comments ###

Comments are stored according to the comment metadata specification (see [/doc/METADATA.ini](/doc/METADATA.ini) for more information)

### Multi-Line Comments ###

Since line breaks are preserved, you can identify multi line comments
by their trailing line break.


## Restrictions ##

The ordering of the hosts is stored in metakeys of type "order".
The value is an ascending number. Ordering of aliases is NOT preserved.

## Examples ##

Mount the plugin:

    $ kdb mount --with-recommends /etc/hosts system/hosts hosts

Print out all known hosts and their aliases:

    $ kdb ls system/hosts

Get IP address of ipv4 host "localhost":

    $ kdb get system/hosts/ipv4/localhost

Check if a comment is belonging to host "localhost":

    $ kdb lsmeta system/hosts/ipv4/localhost

Try to change the host "localhost", should fail because it is not an ipv4 adress:

    $ kdb set system/hosts/ipv4/localhost ::1

```sh
# Backup-and-Restore:/examples/hosts

sudo kdb mount --with-recommends hosts /examples/hosts hosts

# Create hosts file for testing
cat > `kdb file /examples/hosts` << EOF \
127.0.0.1	localhost\
::1	localhost

# Check the file
cat `kdb file /examples/hosts`
#> 127.0.0.1	localhost
#> ::1	localhost

# Check if the values are read correctly
kdb get /examples/hosts/ipv4/localhost
#>127.0.0.1
kdb get /examples/hosts/ipv6/localhost
#>::1

# Should both fail with error 51 and return 5 
kdb set /examples/hosts/ipv4/localhost ::1
# RET:5
# ERRORS:51
kdb set /examples/hosts/ipv6/localhost 127.0.0.1
# RET:5
# ERRORS:51

# cleanup
kdb rm -r /examples/hosts
sudo kdb umount /examples/hosts
```
