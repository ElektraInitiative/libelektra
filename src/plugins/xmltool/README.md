- infos = Information about xmltool plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/xml
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = productive tested/unit memleak experimental obsolete discouraged
- infos/description = Storage using legacy XML format.

## Introduction

This plugin is a storage plugin allowing Elektra to read and write XML
formatted files. It uses the legacy Elektra 0.7 XML format.

This plugin can be used for migration of Key Databases
from 0.7 -> 0.8. It should not be used otherwise.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-xmltool`.

## Dependencies

- `libxml2-dev`

## Restrictions

- only supports metadata as defined in Elektra 0.7
- null and empty values are not distinguished
- exported relative to first key found, not to parent key (ksGetCommonParentName)
- error messages vague (no difference between error opening file and validation errors)

## Examples

After you have upgraded Elektra, you can import XML files from Elektra 0.7:

```sh
kdb import system:/ xmltool < system.xml
kdb import user:/ xmltool < user.xml
```

Or you can also mount an XML file using `xmltool` (not recommended):

```sh
kdb mount /etc/example.xml system:/example xmltool
```
