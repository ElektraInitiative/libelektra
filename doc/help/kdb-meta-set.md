# kdb-meta-set(1) -- Set the value of a metakey

## SYNOPSIS

`kdb meta-set <key name> <metaname> [<metavalue>]`

Where `key name` is the name of the key that the metakey is associated with,
`metaname` is the name of the metakey the user would like to set the value of (or create),
and `metavalue` is the value the user wishes to set the metakey to.
If no `metavalue` is given, the metakey will be removed.

## DESCRIPTION

This command allows the user to set the value of an individual metakey.
If a (non-cascading) key does not already exist and the user tries setting a metakey associated with it, the key will be created with a null value.
If a cascading key is given that does not resolve to an actual key, the operation is aborted.

There is some special handling for the metadata atime, mtime and ctime. They will be converted to time_t.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.
- `-q`, `--quiet`:
  Suppress non-error messages.
- `-f`, `--force`:
  Do not perform cascading KDB operations if the key provided has a namespace. For example, this bypasses validation specified in the spec: namespace for the given key.

## RETURN VALUES

This command will return the following values as an exit status:<br>

- 0:
  No errors.
- 11:
  Key not found. (Invalid `path`)
- 12:
  Setting a non-existing key without a namespace is not possible\.

## KDB

- `/sw/elektra/kdb/#0/current/verbose`:
  Same as `-v`: Explain what is happening.

- `/sw/elektra/kdb/#0/current/quiet`:
  Same as `-q`: Suppress default messages.

## EXAMPLES

To set a metakey called `description` associated with the key `user:/example/key` to the value `Hello World!`:<br>
`kdb meta-set spec:/example/key description "Hello World!"`

To create a new key `spec:/example/newkey` with a null value (if it did not exist before)
and a metakey `namespace/#0` associated with it to the value `system`:<br>
`kdb meta-set /example/newkey "namespace/#0" system`

To create an override link for a `/test` key:

```sh
kdb set /overrides/test "example override"
sudo kdb meta-set spec:/test override/#0 /overrides/test
```

To remove it:

```sh
sudo kdb meta-set spec:/test override/#0
```

## SEE ALSO

- How to get metadata: [kdb-meta-get(1)](kdb-meta-get.md)
- [elektra-metadata(7)](elektra-metadata.md) for an explanation of the metadata concepts.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
