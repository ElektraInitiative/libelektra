install-config-file(1) -- Install configuration files in Elektra

## SYNOPSIS

`install-config-file <elektra path> <config file> [<format>]`

`<elektra path>` is a path in Elektra
`<config file>` is some file on the filesystem

`line` will be used if `<format>` is unspecified

The script has to be called as administrator

## DESCRIPTION

This script installs or merges configuration files from the file system into
Elektra.

If `<elektra path>` is empty then the script
1. Copies `<config file>` into a special path to preserve origin version.
2. Mounts `<config file>` into `<elektra path>` as our version.

If `<elektra path>` already contains data then the script performs a three-way
merge between the file at `<config file>`, the our version at `<elektra path>`
and the preserved origin version from 1.
