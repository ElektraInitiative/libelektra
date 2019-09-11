# kdb-fstab(1) -- Create a new fstab entry

## SYNOPSIS

`kdb fstab <path> <fs_spec> <fs_file> <fs_vfstype> <fs_mntops>`

Where `path` is the path where the created keys should be stored in the key database.
The other arguments are as described in fstab(5).

## DESCRIPTION

This command is used to create a new `fstab` entry.<br>
Due to the unique format of `fstab` entries, it is not possible to set individual elements of new `fstab` entries.<br>
This command is used to gather all the data needed to create a single `fstab` entry in order to bypass the issue of not being able to set individual elements.<br>
This command will create the new entry with a name of `ZZZNewFstabName` which should then automatically be renamed by the `fstab` plugin.<br>

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
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## EXAMPLES

To create an fstab entry to mount `dev/sdb1` to `/media/external` stored in the key `system/fstab/sdb1`:<br>
`kdb fstab system/fstab/sdb1 /dev/sdb1 /media/external ext4 errors=remount-ro`

## SEE ALSO

- Use `kdb info fstab` to get information about the fstab plugin.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
