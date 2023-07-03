- infos = Information about the lineendings plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>, Michael Langhammer <e1125605@student.tuwien.ac.at>, Florian Lindner <florian.lindner@student.tuwien.ac.at>
- infos/maintainer = Florian Lindner <florian.lindner@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/placements = pregetstorage postgetstorage precommit
- infos/status = maintained unittest nodep configurable nodoc
- infos/description = verifies line endings of files

## Introduction

The plugin provides validation for the line endings of a file.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.

## Usage

The plugin checks the line endings of a file.
It validates the line endings against consistency and if the key `/valid` is present also against the specified line ending.
Inconsistent line endings or line endings that don't match `/valid` yield an error in case of `kdbSet`.
In the case of `kdbGet` the plugin yields a warning instead.

## Configuration

The key `/valid` tells the plugin to reject all line endings other than specified and can have the following options:

- `CRLF`: Carriage return followed by a line feed
- `LFCR`: Line feed followed by a carriage return
- `CR`: Carriage return only
- `LF`: Line feed only

If the key doesn't exist only inconsistent line endings are rejected.
