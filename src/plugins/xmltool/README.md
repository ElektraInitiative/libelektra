- infos = Information about xmltool plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage xml
- infos/needs = 
- infos/placements = getstorage setstorage
- infos/status = maintained unittest old
- infos/description = Storage using libelektratools xml format.

## Introduction ##

This plugin is a storage plugin allowing Elektra to read and write xml
formatted files. It uses the libelektratools xml format.

This plugin can be used for migration of Key Databases,
e.g. from 0.7 -> 0.8.

## Dependencies ##

- `libxml2-dev`

## Restrictions ##

This plugin has a difficult time recognizing whether a key has a null
value or an empty value.

## Examples ##

Mount an xml file using `xmltools`:

	kdb mount /etc/example.xml system/example xmltool

Using an xml as export format:

	kdb export system/example xmltool > backup.xml
