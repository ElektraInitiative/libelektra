kdb-get(1) -- Get the value of a key stored in the key database
================================================================

## SYNOPSIS

`kdb get <key name>`

Where `key name` is the name of the key.


## DESCRIPTION

This command is used to retrieve the value of a key.

If you enter a `key name` starting with a leading `/`, then a cascading lookup will be performed in order to attempt to locate the key.
In this case, using the `-v` option allows the user to see the full key name of the key if it is found.


## LIMITATIONS

Only keys within the mountpoint or below the `<key name>` will be considered during a cascading lookup.
A workaround is to pass the `-a` option.
Use the command `kdb get -v <key name>` to see if an override or a fallback was considered by the lookup.



## RETURN VALUES

This command will return the following values as an exit status:

- 0:
  No errors.
- 1-10:
  standard exit codes, see [kdb(1)](kdb.md)
- 11:
  No key found.


## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-a`, `--all`:
  Consider all of the keys.
- `-n`, `--no-newline`:
  Suppress the newline at the end of the output.
- `-v`, `--verbose`:
  Explain what is happening.
  Gives a complete trace of all tried keys.
  Very useful to debug fallback and overrides.


## EXAMPLES

To get the value of a key:
`kdb get user/example/key`

To get the value of a key using a cascading lookup:
`kdb get /example/key`

To get the value of a key without adding a newline to the end of it:
`kdb get -n /example/key`

To explain why a specific key was used (for cascading keys):
`kdb get -v /example/key`

To use bookmarks:
`kdb get +kdb/format`

This command will actually get `user/sw/elektra/kdb/#0/current/format` if the bookmarks commands from
[kdb-set(1)](kdb-set.md) man pages are executed before.

## SEE ALSO

- [kdb(1)](kdb.md) for how to configure the kdb utility and use the bookmarks.
- For more about cascading keys see [elektra-cascading(7)](elektra-cascading.md)
- To get keys in shell scripts, you also can use [kdb-sget(1)](kdb-sget.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
