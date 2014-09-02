- infos = All information you want to know
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage
- infos/placements = setstorage getstorage
- infos/needs =
- infos/recommends =
- infos/description = Includes constants information into the key database.

The plugin is readonly.

## Usage ##

To mount it, use

	kdb mount -R noresolver none system/constants constants

To list all constants, use:

	kdb ls system/constants

