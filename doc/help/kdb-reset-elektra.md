# kdb-reset-elektra(1) - Resets system:/elektra

## SYNOPSIS

`kdb reset-elektra -f`

## DESCRIPTION

Resets all configuration changes that influence Elektra's core, i.e.,
keys in `system:/elektra`.

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
