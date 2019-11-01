install-config-file(1) -- Install configuration files in Elektra

## NAME

`install-config-file` -- Install configuration files in Elektra

## SYNOPSIS

`install-config-file <elektra path> <config file> [<format>]`

`<elektra path>` is a path in Elektra
`<config file>` is some file on the filesystem

`line` will be used if `<format>` is unspecified

The script has to be called as administrator (e.g. with `sudo`).

## DESCRIPTION

This script installs or merges configuration files from the file system into
Elektra. There are two possible scenarios:

1. You use the script for the first time for a file. Then `<elektra path>` is empty the script

   1. Copies `<config file>` into a special path to preserve origin version.
   2. Mounts `<config file>` into `<elektra path>` as our version. This version can then be safely modified.

2. You have already used the script for a previous version of the file. In this case `<elektra path>` already contains data the script performs a three-way
   merge (using `kdb cmerge`) between the file at `<config file>` (their), the `<elektra path>` (our)
   and the preserved key set (base) from step 1 in scenario 1.
