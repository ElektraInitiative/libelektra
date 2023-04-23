# kdb-set(1) -- Set the value of a key

## SYNOPSIS

`kdb set <key name> <value>`

Where `key name` is the name of the key you wish to set the value of (or create) and `value` is the value you would like to set the key to.

## DESCRIPTION

This command allows the user to set the value of an individual key.
If a cascading key is used that does not resolve to an existing key, the operation is aborted as it is ambiguous.

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
- `--`:
  Do not process any following arguments starting with `-` as options.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.
- `-f`, `--force`:
  Do not perform a cascading lookup if the key provided has a namespace. For example, this bypasses validation specified in the spec: namespace for the given key.

## RETURN VALUES

This command will return the following values as an exit status:<br>

- 0:
  No errors.
- 11:
  Not a valid keyname.
- 12:
  Setting a non-existing key without a namespace is not possible.

## KDB

- `/sw/elektra/kdb/#0/current/verbose`:
  Same as `-v`: Explain what is happening.

- `/sw/elektra/kdb/#0/current/quiet`:
  Same as `-q`: Suppress default messages.

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
