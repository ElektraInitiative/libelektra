- infos = Information about the uname plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage info
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = maintained unittest shelltest concept
- infos/description = Includes uname information into the key database.

## Introduction ##

This plugin is a storage plugin that will use the syscall `uname (2)`.
No resolver is needed for that plugin to work.


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
`kdb mount -R noresolver none system/uname uname`
