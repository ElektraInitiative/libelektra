- infos = Information about the len plugin is in keys below
- infos/author = Philipp Oppel <philipp.oppel@tuwien.ac.at>
- infos/maintainer = Florian Lindner <florian.lindner@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements = presetstorage postgetstorage
- infos/status = unittest nodep
- infos/metadata = check/length/max
- infos/description = validates if input is less or equal to length and throws error otherwise

# Length Validation

## Introduction

This plugins purpose is to check the maximum length of strings. For example if `check/length/max` is set to 3, Strings with more than 3 characters will not validate (e.g. "abcd"), whereas "abc" would validate.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

```sh
# Mount `length` plugin to cascading namespace `/tests/length`
kdb mount config.dump /tests/length length

# Check the validity of the string stored in `/tests/length/text`
kdb meta-set spec:/tests/length/text check/length/max 3

# Try to set a longer string
kdb set user:/tests/length/text abcd
# STDERR: .*Validation Semantic.*
# ERROR:  C03200
# RET:    5

# Set a correct string
kdb set user:/tests/length/text abc
kdb get user:/tests/length/text

# Undo modifications to the database
kdb rm -rf /tests/length
kdb umount /tests/length
```

## Limitations

The plugin only checks that strings are not longer than a given number. It is not possible to set a minimum length.
