# kdb-editor(1) -- Use your editor for editing KDB

## SYNOPSIS

`kdb editor <path> [<format>]`

Where `path` is the destination where the user wants to edit keys and `format` is the format in which the keys should be edited.
If the `format` argument is not passed, then the default format will be used as determined by the value of the `sw/kdb/current/format` key.
By default, that key contains `storage`.
The `storage` plugin can be configured at compile-time or changed by the link `libelektra-plugin-storage.so`.
The `format` attribute relies on Elektra’s plugin system to properly import the configuration. The user can view all plugins available for use by running the kdb-plugin-list(1) command.
To learn about any plugin, the user can simply use the kdb-plugin-info(1) command.

## DESCRIPTION

This command allows a user to edit configuration of the key database using a text editor.
The user should specify the format that the current configuration or keys are in, otherwise the default format will be used.

## RETURN VALUES

- 0:
  successful.
- 1-10:
  standard exit codes, see [kdb(1)](kdb.md)
- 11:
  could not export configuration.
- 12:
  No namespace specified.
- 13:
  could not import configuration because of conflicts
- 14:
  could not import configuration because of error during kdbSet()
  (Most likely a validation error)
- 15:
  could not start editor.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-s`, `--strategy <name>`:
  Specify which strategy should be used to resolve conflicts.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.
- `-e`, `--editor <editor>`:
  Which editor to use.

## Strategies

The strategies implemented by the merge framework and are documented in
[elektra-merge-strategy(7)](elektra-merge-strategy.md).

## KDB

- `/sw/elektra/kdb/#0/current/format`:
  The default format if none given. Defaults to `storage` if the key does not exist.

- `/sw/elektra/kdb/#0/current/editor`:
  The default editor, if no `-e` option is given.
  Defaults to `/usr/bin/sensible-editor`, `/usr/bin/editor` or `/usr/bin/vi` if the key does not exist.

## EXAMPLES

To change the configuration in KDB below `user:/ini` with `/usr/bin/vim`, you would use:<br>
`kdb editor -e /usr/bin/vim user:/ini`

Or set a new editor as default using:<br>
`kdb set /sw/elektra/kdb/#0/current/editor /usr/bin/nano`

To change the configuration in KDB below `user` in xml format, you would use:<br>
`kdb editor user xml`

## SEE ALSO

- [elektra-merge-strategy(7)](elektra-merge-strategy.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
