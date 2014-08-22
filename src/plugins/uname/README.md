- infos = Information about the uname plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/recommends = 
- infos/description = Includes uname information into the key database.

## Introduction ##

This plugin is a storage plugin that takes `/etc/uname` file as its backend storage. The `kdbGet()` method will parse `/etc/uname` and generate a  valid key tree. The `kdbSet()` method will take a KeySet with valid filesystem keys and print an equivalent regular uname in stdout. 

## Special Values ##

This plugin defines following keynames below its mountpoint:		
	sysname
	nodename
	release
	version
	machine

## Restrictions ##

This plugin is read-only.

## Example ##

To mount uname information using this plugin:		
	kdb mount -R noresolver none system/uname uname
