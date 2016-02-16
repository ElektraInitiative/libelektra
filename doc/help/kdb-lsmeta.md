kdb-lsmeta(1) - Print meta keys associated with a key
=====================================================

## SYNOPSIS

`kdb lsmeta <key-name>`

## DESCRIPTION

This command prints the names of all meta keys associated with a given key.  
If no meta keys are associated with the given key, nothing will be printed.  

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


## EXAMPLE

To see which meta keys are associated with a key:  
	`kdb lsmeta /example/key`

## SEE ALSO

- [elektra-meta-data(7)](elektra-meta-data.md)
