- infos = Information about the lineendings plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = pregetstorage precommit
- infos/status = maintained unittest nodep configurable nodoc
- infos/description = verifies line endings of files

## Introduction

The Lineendings Plugin verifies the line endings of a file.
If inconsistent line endings or line endings that don't match `valid` are detected the plugin yields an error
if it is used during `kdbSet`. In the case of `kdbGet`, the plugin yields a warning instead.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Configuration

`valid`
The key tells the plugin to reject all line endings other than specified in this key. Valid options: CRLF, LFCR, CR, LF
If the key doesn't exist only inconsistent line endings are rejected.
