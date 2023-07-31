# kdb-reset(1) - Resets the whole KDB

## SYNOPSIS

`kdb reset -f`

## DESCRIPTION

Resets the whole KDB.

## WARNING

These changes cannot be undone, please use with care!
This command writes into the `/etc` directory and as such it requires root permissions.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.

## SEE ALSO

- [kdb-backup(1)](kdb-backup.md).
- [kdb-stash(1)](kdb-stash.md).
- [elektra-mounting(7)](elektra-mounting.md).
