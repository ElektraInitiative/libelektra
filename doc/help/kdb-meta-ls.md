# kdb-meta-ls(1) - Print metakeys associated with a key

## SYNOPSIS

`kdb meta-ls <key name>`

Where `key name` is the name of the key.

## DESCRIPTION

This command prints the names of all metakeys associated with the key named `key name`.<br>
If no metakeys are associated with the given key, nothing will be printed.<br>

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-0`, `--null`:
  Use binary 0 termination.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings. Show which key will be used.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## EXAMPLE

To see which metakeys are associated with a key:<br>
`kdb meta-ls /example/key`

## SEE ALSO

- [elektra-metadata(7)](elektra-metadata.md) for an explanation of the metadata concepts.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
