- infos = Information about xmltool plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/xml
- infos/needs = 
- infos/placements = getstorage setstorage
- infos/status = maintained unittest final unfinished old
- infos/description = Storage using libelektratools xml format.

## Introduction ##

This plugin is a storage plugin allowing Elektra to read and write xml
formatted files. It uses the `libelektratools` 0.7 xml format.

This plugin can be used for migration of Key Databases
from 0.7 -> 0.8. It should not be used otherwise.

## Dependencies ##

- `libxml2-dev`

## Restrictions ##

- only supports metadata as defined in Elektra 0.7
- null and empty values are not distinguished
- exported relative to first key found, not to parent key (ksGetCommonParentName)
- error messages vague (no difference between error opening file and validation errors)

## Examples ##

After you have upgraded Elektra, you can import xml files from Elektra 0.7:

    kdb import system xmltool < system.xml
    kdb import user xmltool < user.xml

Or you can also mount an xml file using `xmltool` (not recommended!):

    kdb mount /etc/example.xml system/example xmltool
