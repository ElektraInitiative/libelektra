kdb-lsmeta(1) - Print metakeys associated with a key
=====================================================

## SYNOPSIS

`kdb lsmeta <key name>`

## DESCRIPTION

This command prints the names of all metakeys associated with the key named `key name`.  
If no metakeys are associated with the given key, nothing will be printed.  

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-0`, `--null`:
  Use binary 0 termination.
- `-v`, `--verbose`:
  Show which key will be used.
- `-C`, `--color`=[when]:
  Print never/auto(default)/always colored output.


## EXAMPLE

To see which metakeys are associated with a key:  
`kdb lsmeta /example/key`

## SEE ALSO

- [elektra-metadata(7)](elektra-metadata.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.