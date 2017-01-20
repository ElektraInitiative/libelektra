kdb-rm(1) -- Remove key(s) from the key database
================================================

## SYNOPSIS

`kdb rm <path>`

Where `path` is the path of the key(s) you want to remove.
Note that when using the `-r` flag, not only the key directly at `path` will be removed, but all of the keys below the path as well.

## DESCRIPTION

This command removes key(s) from the Key database.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-r`, `--recursive`:
  Work in a recursive mode.
- `-C`, `--color`=[when]:
  Print never/auto(default)/always colored output.

## EXAMPLES

To remove multiple keys:
`kdb rm -r user/example`

To remove a single key:
`kdb rm user/example/key1`

## SEE ALSO

- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.