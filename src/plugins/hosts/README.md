- infos = Information about HOSTS plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage
- infos/needs =
- infos/recommends = glob network
- infos/placements = getstorage setstorage
- infos/status = maintained unittest nodep libc
- infos/description = This plugin reads and writes /etc/hosts files.


## Introduction ##

The /etc/hosts file is a simple text file that associates IP addresses
with hostnames, one line per IP address. The format is described in hosts(5).

## Special values ##

####Hostnames
Canonical hostnames are stored as key names with the IP address as key
value. 

####Aliases
Aliases are stored as sub keys with a read only duplicate of the
associated ip address as value. 

####Comments
Comments are stored according to the comment metadata specification (see doc/METADATA.ini for more information)

####Multi-Line Comments
Since line breaks are preserved, you can identify multi line comments
by their trailing line break.


## Restrictions##

The ordering of the hosts is stored in meta keys of type "order".
The value is an ascending number. Ordering of aliases is NOT preserved.

##Examples##

Mount the plugin:

    $ kdb mount /etc/hosts system/hosts hosts

Print out all known hosts and their aliases:

    $ kdb ls system/hosts

Get IP address of host "localhost":

    $ kdb get system/hosts/localhost

Fetch comment belonging to host "localhost":

    $ kdb getmeta system/hosts/localhost comment
