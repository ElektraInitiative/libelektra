- infos = Information about the lineendings plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = pregetstorage precommit
- infos/status = maintained unittest nodep configurable nodoc
- infos/description = verifies lineendings of files

## Introduction ##

The Lineendings Plugin verifies the Lineendings of a file.
If inconsistent lineendings or lineendings that don't match `valid` are detected the plugin yields an error.

## Configuration ##

`valid` 
The key tells the plugin to reject all lineendings other than specified in this key. Valid options: CRLF, LFCR, CR, LF
If the key doesn't exist only inconsistent lineendings get rejected. 

