# kdb-meta-get(1) -- Get the value of a metakey stored in the key database

## SYNOPSIS

`kdb meta-get <key name> <metaname>`<br>

Where `key name` is the name of the key and
`metaname` is the name of the metakey the user would like to access.

## DESCRIPTION

This command is used to print the value of a metakey.
A metakey is information stored in a key which describes that key.

The handling of cascading `key name` does not differ to `kdb get`.
Make sure to use the namespace `spec`, if you want metadata from there.

## RETURN VALUES

This command will return the following values as an exit status:<br>

- 0:
  No errors.
- 11:
  Key not found. (Invalid `path`)
- 12:
  Metakey not found. (Invalid `metaname`).

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-n`, `--no-newline`:
  Suppress the newline at the end of the output.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## EXAMPLES

To get the value of a metakey called `description` stored in the key `spec:/example/key`:<br>
`kdb meta-get spec:/example/key description`

To get the value of metakey called `override/#0` stored in the key `spec:/example/dir/key`:<br>
`kdb meta-get spec:/example/dir/key "override/#0"`

## SEE ALSO

- How to set metadata: [kdb-meta-set(1)](kdb-meta-set.md)
- For more about cascading keys see [elektra-cascading(7)](elektra-cascading.md)
- [elektra-metadata(7)](elektra-metadata.md) for an explanation of the metadata concepts.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
