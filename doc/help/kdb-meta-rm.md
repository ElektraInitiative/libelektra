# kdb-meta-rm(1) -- Remove metakey of a key from the key database

## SYNOPSIS

`kdb meta rm <key name> <metaname>`

Where `key name` is the name of the key and `metaname` is the name of the metakey you want to remove.

## DESCRIPTION

This command removes a metakey of a key from the Key database.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
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
  Key not found. (Invalid `path`)
- 12:
  Metaname does not exist

## EXAMPLES

To remove metakey `metakey` of a key:<br>
`kdb meta rm user:/example metakey`

## SEE ALSO

- [elektra-metadata(7)](elektra-metadata.md) for an explanation of the metadata concepts.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
