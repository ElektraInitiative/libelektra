# kdb-umount(1) - Unmount a file from the key database

## SYNOPSIS

`kdb umount <name>`

## DESCRIPTION

Unmount backend from key database.
This command writes into the `/etc` directory and as such it requires root permissions.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Show which mount points were considered.

## SEE ALSO

- [kdb-mount(1)](kdb-mount.md).
- [elektra-mounting(7)](elektra-mounting.md).
