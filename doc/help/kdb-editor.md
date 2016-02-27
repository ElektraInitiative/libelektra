kdb-import(1) -- Import an existing configuration into the key database
=======================================================================

## SYNOPSIS

`kdb editor <key-name> [<format>]`

Where `key-name` is the destination where the user wants to edit keys and `format` is the format in which the keys should be edited.
If the `format` argument is not passed, then the default format will be used as determined by the value of the `sw/kdb/current/format` key. By default, that key is set to the `dump` format.
The `format` attribute relies on Elektra's plugin system to properly import the configuration. The user can view all plugins available for use by running the kdb-list(1) command.
To learn about any plugin, the user can simply use the kdb-info(1) command.

## DESCRIPTION

This command allows a user to edit configuration of the key database using a text editor.
The user should specify the format that the current configuration or keys are in, otherwise the default format will be used.

## KDB

- `/sw/elektra/kdb/#0/current/format`:
  The default format if none given. Defaults to `dump` if the key does not exist.

- `/sw/elektra/kdb/#0/current/editor`:
  The default editor, if no `-e` option is given.
  Defaults to `/usr/bin/sensible-editor`, `/usr/bin/editor` or `/usr/bin/vi` if the key does not exist.


## RETURN VALUES

- 0:
  successful.
- 1-10:
  standard exit codes, see [kdb(1)](kdb.md)
- 11:
  could not export configuration.
- 12:
  could not start editor.
- 13:
  could not import configuration.


## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `s`, `--strategy <name>`:
  Specify which strategy should be used to resolve conflicts.
- `-v`, `--verbose`:
  Explain what is happening.
- `-e`, `--editor <editor>`:
  Which editor to use.


## EXAMPLES

To change the configuration in KDB below `user/ini` with `/usr/bin/vim`, you would use:
`kdb editor -e /usr/bin/vim user/ini`

Or set a new editor as default using:
`kdb set /sw/elektra/kdb/#0/current/editor /usr/bin/nano`

## SEE ALSO

- [elektra-merge-strategy(7)](elektra-merge-strategy.md)
