# kdb-get(1) -- Get the value of a key stored in the key database

## SYNOPSIS

`kdb get <key name>`

Where `key name` is the name of the key.

## DESCRIPTION

This command is used to retrieve the value of a key.

If you enter a `key name` starting with a leading `/`, then a cascading lookup will be performed in order to attempt to locate the key.
In this case, using the `-v` option allows the user to see the full key name of the key if it is found.

## LIMITATIONS

Only keys within the mount point or below the `<key name>` will be considered during a cascading lookup.
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
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## EXAMPLES

```sh
# Backup-and-Restore: user:/tests/get/examples

# We use the `dump` plugin, since some storage plugins, e.g. INI,
# create intermediate keys.
sudo kdb mount get.ecf user:/tests/get/examples/kdb-get dump
sudo kdb mount get.ecf spec:/tests/get/examples/kdb-get dump

# Create the keys we use for the examples
kdb set user:/tests/get/examples/kdb-get/key myKey
kdb meta-set spec:/tests/get/examples/kdb-get/anotherKey default defaultValue

# To get the value of a key:
kdb get user:/tests/get/examples/kdb-get/key
#> myKey

# To get the value of a key using a cascading lookup:
kdb get /tests/get/examples/kdb-get/key
#> myKey

# To get the value of a key without adding a newline to the end of it:
kdb get -n /tests/get/examples/kdb-get/key
#> myKey

# To explain why a specific key was used (for cascading keys):
kdb get -v /tests/get/examples/kdb-get/key
#> got 3 keys
#> searching spec:/tests/get/examples/kdb-get/key, found: <nothing>, options: KDB_O_CALLBACK
#>     searching proc:/tests/get/examples/kdb-get/key, found: <nothing>, options: KDB_O_CALLBACK
#>     searching dir:/tests/get/examples/kdb-get/key, found: <nothing>, options: KDB_O_CALLBACK
#>     searching user:/tests/get/examples/kdb-get/key, found: user:/tests/get/examples/kdb-get/key, options: KDB_O_CALLBACK
#> The resulting keyname is user:/tests/get/examples/kdb-get/key
#> The resulting value size is 6
#> myKey

# Output if only a default value is set for a key:
kdb get -v /tests/get/examples/kdb-get/anotherKey
#> got 3 keys
#> searching spec:/tests/get/examples/kdb-get/anotherKey, found: spec:/tests/get/examples/kdb-get/anotherKey, options: KDB_O_CALLBACK
#> The key was not found in any other namespace, taking the default
#> The resulting keyname is default:/tests/get/examples/kdb-get/anotherKey
#> The resulting value size is 13
#> defaultValue

kdb rm user:/tests/get/examples/kdb-get/key
kdb rm spec:/tests/get/examples/kdb-get/anotherKey
sudo kdb umount user:/tests/get/examples/kdb-get
sudo kdb umount spec:/tests/get/examples/kdb-get
```

To use bookmarks:<br>
`kdb get +kdb/format`

This command will actually get `user:/sw/elektra/kdb/#0/current/format` if the bookmarks commands from
[kdb-set(1)](kdb-set.md) man pages are executed before.

## SEE ALSO

- [kdb(1)](kdb.md) for how to configure the kdb utility and use the bookmarks.
- For more about cascading keys see [elektra-cascading(7)](elektra-cascading.md)
- To get keys in shell scripts, you also can use [kdb-sget(1)](kdb-sget.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
