# kdb-change-resolver-symlink -- Changes the default resolver symlink

## SYNOPSIS

`kdb change-resolver-symlink <resolver>`

Where `resolver` is the name of the new default resolver plugin.

## DESCRIPTION

This command updates the symlink pointing to the default resolver if `resolver` is a valid resolver plugin.

## EXAMPLES

Set default resolver plugin to resolver_fm_hpu_b:<br>
`kdb change-resolver-symlink resolver_fm_hpu_b`

## SEE ALSO

- [kdb-change-storage-symlink(1)](kdb-change-storage-symlink.md)
