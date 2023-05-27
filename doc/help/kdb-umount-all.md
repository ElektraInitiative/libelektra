# kdb-umount-all(1) - Unmount everything in the key database

## SYNOPSIS

`kdb umount-all -f`

## DESCRIPTION

Unmount all backend and all global plugins from key database.

## WARNING

These changes cannot be undone, please use with care!
This command writes into the `/etc` directory and as such it requires root permissions.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.

## SEE ALSO

- [kdb-mount(1)](kdb-mount.md).
- [elektra-mounting(7)](elektra-mounting.md).
