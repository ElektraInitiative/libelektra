- infos = Information about SIMPLEINI plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs = code null
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = Very simple storage which writes out in a basic ini format.


## Introduction ##

This plugin reads and writes files written in a basic ini format.
It is very simplistic, the [ini](../ini/) plugin and for specifications the [ni](../ni/) plugin
should be preferred.

## Restrictions ##

This plugin can work with files in a simple ini format. This plugins needs the code and null plugins. 
A code plugin is used for the escape character for special symbols and the null plugin
is used to handle null values.

##Examples##

Mount the plugin:

    $ kdb mount -d /etc/samba/smb.conf system/samba ccode null simpleini
