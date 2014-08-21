- infos = Information about xmltool plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs = 
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/description = Storage using libelektratools xml format.

## Introdcution ##

This plugin is a storage plugin allowing Elektra to read and write xml formatted files. It uses the libelektratools xml format.

## Restrictions ## 

This plugin has a difficult time reconginzing whether a key has a null value or an empty value.

## Examples ##

Mount an xml file using `xmltools`:
	kdb mount /etc/example.xml system/example xmltool
