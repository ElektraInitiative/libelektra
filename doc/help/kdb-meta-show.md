# kdb-meta-show(1) -- Print all metakeys along with their value

## SYNOPSIS

`kdb meta show <key name>`<br>

Where `key name` is the name of the key the user would like to access.

## DESCRIPTION

This command is used to print all metakeys and its values for a given key.
A metakey is information stored in a key which describes that key.

The handling of cascading `key name` does not differ to `kdb get`.
Make sure to use the namespace `spec`, if you want metadata from there.

## RETURN VALUES

This command will return the following values as an exit status:<br>

- 0:
  No errors.
- 1:
  Key not found. (Invalid `path`)

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

## RETURN VALUES

This command will return the following values as an exit status:<br>

- 0:
  No errors.
- 11:
  Key not found.

## EXAMPLES

```sh
# Backup-and-Restore: user:/tests/examples

# We use `dump` as storage format here
sudo kdb mount ls.ecf user:/tests/examples dump

# Create the keys we use for the examples
kdb set user:/tests/examples/kdb-meta-show test
kdb meta set user:/tests/examples/kdb-meta-show meta1 val1
kdb meta set user:/tests/examples/kdb-meta-show meta2 val2
kdb meta set user:/tests/examples/kdb-meta-show meta3 val3
kdb meta set user:/tests/examples/kdb-meta-show meta4 val4

# list all meta keys for /tests/examples/kdb-meta-show
kdb meta show /tests/examples/kdb-meta-show
#> meta1: val1
#> meta2: val2
#> meta3: val3
#> meta4: val4

kdb rm -r user:/tests/examples
sudo kdb umount user:/tests/examples
```

## SEE ALSO

- How to set metadata: [kdb-meta-set(1)](kdb-meta-set.md)
- For more about cascading keys see [elektra-cascading(7)](elektra-cascading.md)
- [elektra-metadata(7)](elektra-metadata.md) for an explanation of the metadata concepts.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
