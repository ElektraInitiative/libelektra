- infos = CLI program for interacting with the keystore
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/maintainer = Hannes Laimer <hannes.laimer@tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = cli
- infos/status = maintained nodep global
- infos/description = cli interface for keystore

## Purpose

Allow interaction with the keystore using the command line.

## Installation

Installed with libelektra.

## Usage

Prints every added, changed or deleted key on the console.
To use it, add it during mounting:

```sh
# set a key value
kdb set user:/a/b testvalue

# get the valuie of a key
kdb get user:/a/b # -> testvalue

# list available commands
kdb --help

# get information about a specific command
kdb meta set --help # -> show how to use `meta set`
```
