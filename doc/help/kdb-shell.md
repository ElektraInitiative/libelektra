# kdb-shell(1) -- Start a kdb shell instance

## SYNOPSIS

`kdb shell`

## DESCRIPTION

This command is used to start an instance of the kdb shell.<br>
The kdb shell allows for a user to interactively view, edit, or otherwise work with the key database.<br>

## SHELL COMMANDS

The kdb shell offers a number of commands to interact with the key database.

- `kdbGet <name>`: Get the value of a key.

- `kdbSet <name>`:
  Set the value of a key.
- `keySetName <name>`:
  Set the name of the current key.
- `keySetMeta <name> <string>`:
  Set a metakey associated with the current key.
- `keySetString <string>`:
  Set a string value for the current key.
- `ksAppendKey`:
  Append the current key to the current keyset.
- `ksCut <name>`:
  Cut the current keyset.
- `ksOutput`:
  Prints the keys in the current keyset.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.

## EXAMPLES

To execute commands from a textfile, you can use:<br>
`cat commands.txt | kdb shell`

To have readline functionality (line editing, history, ...), you can use:<br>
`rlwrap kdb shell`

## SEE ALSO

To learn more about these commands and how they work, refer to the [Elektra API Documentation](https://doc.libelektra.org/api/latest/html).
