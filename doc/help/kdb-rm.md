# kdb-rm(1) -- Remove key(s) from the key database

## SYNOPSIS

`kdb rm <path>`

Where `path` is the path of the key(s) you want to remove.
Note that when using the `-r` flag, not only the key directly at `path` will be removed, but all of the keys below the path as well.

## DESCRIPTION

This command removes key(s) from the Key database.

## RETURN VALUES

This command will return the following values as an exit status:

- 0:
  No errors.
- 1-10:
  standard exit codes, see [kdb(1)](kdb.md)
- 11:
  No key to remove found.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-r`, `--recursive`:
  Work in a recursive mode.
- `-E`, `--without-elektra`:
  Omit the `system:/elektra` directory.
- `-f`, `--force`:
  Do not fail on missing key, nor print if there was a key (-v to still print).
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## EXAMPLES

To remove a single key:<br>
`kdb rm user:/example/key1`

To remove multiple keys:<br>
`kdb rm -r user:/example`

To remove all keys in `system` except `system:/elektra`:<br>
`sudo kdb rm -rE system`

To not fail when key is missing:<br>
`kdb rm -f user:/maybe/missing`

## SEE ALSO

- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
