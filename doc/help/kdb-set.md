# kdb-set(1) -- Set the value of a key

## SYNOPSIS

`kdb set <key name> <value>`

Where `key name` is the name of the key you wish to set the value of (or create) and `value` is the value you would like to set the key to.

## DESCRIPTION

This command allows the user to set the value of an individual key.

## EMPTY VALUES

To set a key to an empty value, `""` should be passed for the `value` argument.

## NEGATIVE VALUES

To set a key to a negative value, `--` has to be used to stop option processing. (see example below)

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-q`, `--quiet`:
  Suppress non-error messages.
- `-N`, `--namespace=NS`:
  Specify the namespace to use when writing cascading keys.
  See [below in KDB](#KDB).
- `--`:
  Do not process any following arguments starting with `-` as options.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## KDB

- `/sw/elektra/kdb/#0/current/verbose`:
  Same as `-v`: Explain what is happening.

- `/sw/elektra/kdb/#0/current/quiet`:
  Same as `-q`: Suppress default messages.

- `/sw/elektra/kdb/#0/current/namespace`:
  Specifies which default namespace should be used when setting a cascading name.
  By default the namespace is user, except `kdb` is used as root, then `system`
  is the default.

## EXAMPLES

To set a Key to the value `Hello World!`:<br>
`kdb set user:/example/key "Hello World!"`

To set a key to an empty value:<br>
`kdb set user:/example/key ""`

To set a key to a negative value:<br>
`kdb set -- /tests/neg -3`

To create bookmarks:<br>
`kdb set user:/sw/elektra/kdb/#0/current/bookmarks ""`

Followed by:<br>
`kdb set user:/sw/elektra/kdb/#0/current/bookmarks/kdb user:/sw/elektra/kdb/#0/current`

## SEE ALSO

- [kdb(1)](kdb.md) for how to configure the kdb utility and use the bookmarks.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
- [elektra-values(7)](elektra-values.md) for the difference between empty and null values.
