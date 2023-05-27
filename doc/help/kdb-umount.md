# kdb-umount(1) - Unmount a file from the key database

## SYNOPSIS

`kdb umount <name>`

## DESCRIPTION

Unmount backend from key database.
This command writes into the `/etc` directory and as such it requires root permissions.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Show which mount points were considered.

## RETURN VALUES

This command will return the following values as an exit status:<br>

- 0:
  No errors.
- 11:
  Mountpoint does not exist.

## SEE ALSO

- [kdb-mount(1)](kdb-mount.md).
- [elektra-mounting(7)](elektra-mounting.md).
