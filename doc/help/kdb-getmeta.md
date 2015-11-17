kdb-getmeta(1) -- Get the value of a meta key stored in the key database
========================================================================

## SYNOPSIS

`kdb getmeta <path> <meta-name>`  

Where `path` is the full path to the key and `meta-name` is the name of the meta key the user would like to access.  

## DESCRIPTION

This command is used to print the value of a meta key.  
A meta key is information stored in a key which describes that key.  

## RETURN VALUES

This command will return the following values as an exit status:  
* 0:
  No errors.  
* 1:
  Key not found. (Invalid `path`)  
* 2:
  Meta key not found. (Invalid `meta-name`).  


## OPTIONS

- `-H`, `--help`:
  Print help text.
- `-V`, `--version`:
  Print version info.
- `-n`, `--no-newline`:          Suppress the newline at the end of the output.

## EXAMPLES

To get the value of a meta key called `info` stored in the key `user/example/key`:  
	`kdb getmeta user/example/key info`  

## SEE ALSO

- [elektra-meta-data(7)](elektra-meta-data.md)
