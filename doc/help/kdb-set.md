kdb-set(1) -- Set the value of a key
====================================

## SYNOPSIS

`kdb set <key-name> [<value>]`

Where `key-name` is the path to the key you wish to set the value of (or create) and `value` is the value you would like to set the key to.
If the `value` argument is not passed, the key will be set to a value of `null`.

## DESCRIPTION

This command allows the user to set the value of an individual key.

## EMPTY VALUES

To set a key to an empty value, `""` should be passed for the `value` argument.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-v`, `--verbose`:
  Explain what is happening.
- `-N`, `--namespace`=<ns>:
  Specify the namespace to use when writing cascading keys.
  See [below in KDB](#KDB).

## KDB

- `/sw/elektra/kdb/#0/current/namespace`:
  Specifies which default namespace should be used when setting a cascading name.
  By default the namespace is user, except `kdb` is used as root, then `system`
  is the default.


## EXAMPLES

To set a Key to the value `Hello World!`:
`kdb set user/example/key "Hello World!"`

To create a new key with a null value:
`kdb set user/example/key`

To set a key to an empty value:
`kdb set user/example/key ""`

To create bookmarks:
`kdb set user/sw/elektra/kdb/#0/current/bookmarks`
followed by:
`kdb set user/sw/elektra/kdb/#0/current/bookmarks/kdb user/sw/elektra/kdb/#0/current`


## SEE ALSO

- [kdb(1)](kdb.md) for how to configure the kdb utility and use the bookmarks.
- For difference between empty and null values, see [elektra-values(7)](elektra-values.md)
