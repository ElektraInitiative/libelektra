kdb-setmeta(1) -- Set the value of a metakey
=============================================

## SYNOPSIS

`kdb setmeta <key-name> <meta-name> <meta-value>`

Where `key-name` is the path to the key that the metakey is associated with,
`meta-name` is the name of the metakey the user would like to set the value of (or create),
and `meta-value` is the value the user wishes to set the metakey to.

## DESCRIPTION

This command allows the user to set the value of an individual metakey.
If a key does not already exist and the user tries setting a metakey associated with it, the key will be created with a null value.
There is some special handling for the meta data atime, mtime and ctime. They will be converted to time_t.

For cascading keys, the namespace will default to `spec`, because
that is the place where you usually want to set meta data.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-v`, `--verbose`:
  Explain what is happening.
- `-q`, `--quiet`:
  Suppress non-error messages.
- `-C`, `--color`=[when]:
  Print never/auto(default)/always colored output.

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

To set a metakey called `description` associated with the key `user/example/key` to the value `Hello World!`:
`kdb setmeta spec/example/key description "Hello World!"`

To create a new key `spec/example/newkey` with a null value (if it did not exist before)
and a metakey `namespace/#0` associated with it to the value `system`:
`kdb setmeta /example/newkey "namespace/#0" system`

To create an override link for a `/test` key:

	kdb set /overrides/test "example override"
	sudo kdb setmeta spec/test override/#0 /overrides/test

## SEE ALSO

- How to get meta data: [kdb-getmeta(1)](kdb-getmeta.md)
- [elektra-meta-data(7)](elektra-meta-data.md)
