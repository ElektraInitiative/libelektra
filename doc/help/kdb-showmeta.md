# kdb-showmeta(1) -- Print all metakeys along with their value

## SYNOPSIS

`kdb showmeta <key name>`<br>

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

## EXAMPLES

To get all metadata key and value information of the key `spec/example/key`:<br>
`kdb showmeta spec/example/key`

## SEE ALSO

- How to set metadata: [kdb-setmeta(1)](kdb-setmeta.md)
- For more about cascading keys see [elektra-cascading(7)](elektra-cascading.md)
- [elektra-metadata(7)](elektra-metadata.md) for an explanation of the metadata concepts.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
