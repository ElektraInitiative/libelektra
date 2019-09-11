# kdb-change-storage-symlink -- Changes the default storage symlink

## SYNOPSIS

`kdb change-storage-symlink <storage>`

Where `storage` is the name of the new default storage plugin.

## DESCRIPTION

This command updates the symlink pointing to the default storage if `storage` is a valid storage plugin.

## EXAMPLES

Set default storage plugin to ini:<br>
`kdb change-storage-symlink ini`

## SEE ALSO

- [kdb-change-resolver-symlink(1)](kdb-change-resolver-symlink.md)
