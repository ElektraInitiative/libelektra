- infos = Information about SIMPLEINI plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage ini
- infos/needs = code null
- infos/placements = getstorage setstorage
- infos/status = maintained unfinished concept 1000
- infos/description = Very simple storage which writes out in a basic ini format.


## Introduction ##

This plugin reads and writes files written in a basic ini format.
It is very simplistic, the [ini](../ini/) plugin and for specifications the [ni](../ni/) plugin
should be preferred.

## Usage

It is quite suitable to export configuration if you want line-by-line key=value pairs.
(Thus +1000 in status)

	$ kdb export system/samba simpleini

## Restrictions ##

This plugin can work with files in a simple ini format. This plugins needs the code and null plugins. 
A code plugin is used for the escape character for special symbols and the null plugin
is used to handle null values.

##Examples##

Mount the plugin:

    $ kdb mount -d /etc/samba/smb.conf system/samba ccode null simpleini
