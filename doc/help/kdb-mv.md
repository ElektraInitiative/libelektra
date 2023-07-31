# kdb-mv(1) -- Move keys within the key database

## SYNOPSIS

`kdb mv <source> <dest>`

Where `source` is the path of the key(s) you want to copy and `dest` is the path where you would like to move the key(s) to.
Note that when using the `-r` flag, `source` as well as all of the keys below it will be moved.

## DESCRIPTION

This command moves key(s) in the Key database.
You can move keys to other paths within the database.

## LIMITATIONS

Neither `source` nor `dest` can be a cascading key.
(Start with `/`).
Make sure to select a namespace.

## RETURN VALUES

This command will return the following values as an exit status:

- 0:
  No errors.
- 1-10:
  standard exit codes, see [kdb(1)](kdb.md)
- 11:
  No key to move found.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-r`, `--recursive`:
  Work in a recursive mode.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## EXAMPLES

To move multiple keys:<br>
`kdb mv -r user:/example1 user:/example2`

To move a single key:<br>
`kdb mv user:/example/key1 user:/example/key2`
