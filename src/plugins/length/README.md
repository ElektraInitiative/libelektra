- infos = Information about the len plugin is in keys below
- infos/author = Philipp Oppel <philipp.oppel@tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained unittest nodep
- infos/metadata = check/length
- infos/description = validates if input is less or equal to length and throws error otherwise

# IP Address Validation

## Introduction

This plugins purpose is to check the maximum length of strings. For example if check/length is set to 3, Strings with more than 3 characters will not validate (e.g. "abcd"), whereas "abc" would validate.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

```sh
# Mount `length` plugin to cascading namespace `/tests/length`
kdb mount config.dump /tests/length dump length

# Check the validity of the string stored in `/tests/length/text`
kdb meta-set /tests/length/text check/length 3

# Try to set a longer string
kdb set /tests/length/text abcd
# STDERR: .*Validation Semantic.*
# ERROR:  C03200
# RET:    5

# Set a correct string
kdb set /tests/length/text abc
kdb get /tests/length/text
#> abc

# Undo modifications to the database
kdb rm -r /tests/length
kdb umount /tests/length
```

## Limitations

The plugin only checks IP addresses for validity. It is not able to resolve hostnames. If you are looking for a plugin that supports hostnames, check out the [network plugin](../network/).
