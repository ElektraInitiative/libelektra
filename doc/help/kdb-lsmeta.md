kdb-lsmeta(1) - Print meta keys associated with a key
=====================================================

## SYNOPSIS

`kdb lsmeta <key-name>`

## DESCRIPTION

This command prints the names of all meta keys associated with a given key.  
If no meta keys are associated with the given key, nothing will be printed.  

## OPTIONS

- `-H`, `--help`:
  Print help text.
- `-V`, `--version`:
  Print version info.
- `-0`, `--null`:
  Use binary 0 termination.


## EXAMPLE

To see which meta keys are associated with a key:  
	`kdb lsmeta user/example/key`  

## SEE ALSO

- [elektra-meta-data(7)](elektra-meta-data.md)
