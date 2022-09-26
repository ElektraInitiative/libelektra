# kdb-install-config-file(1) -- Install configuration files in Elektra

## SYNOPSIS

`kdb install-config-file <elektra path> <config file> [<format>]`

- `<elektra path>` is a path in Elektra
- `<config file>` is some file on the filesystem

`line` will be used as `<format>` if `<format>` is unspecified.

The script has to be called as administrator (e.g. with `sudo`).

## DESCRIPTION

This script installs or merges configuration files from the file system into
Elektra. There are two possible scenarios:

1. You use the script for the first time for a file. Then `<elektra path>` is empty. The script

   1. copies `<config file>` into a special path to preserve origin version.
   2. mounts `<config file>` into `<elektra path>` as our version. This version can then be safely modified.

2. You have already used the script for a previous version of the file. In this case `<elektra path>` already contains data the script performs a three-way
   merge (using `kdb merge`) between the file at `<config file>` (their), the `<elektra path>` (our)
   and the preserved key set (base) from step 1 in scenario 1.

## EXAMPLES

To install the config file at `~/.config/installing.ini` we can use the following command

`kdb install-config-file user:/tests/installing installing.ini ini`
