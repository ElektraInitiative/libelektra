kdb-set(1) -- Set the value of a key
====================================

## SYNOPSIS

`kdb set <key-name> [<value>]`  

Where `key-name` is the path to the key you wish to set the value of (or create) and `value` is the value you would like to set the key to.  
If the `value` argument is not passed, the key will be set to a value of `null`.  

## DESCRIPTION

This command allows the user to set the value of an individual key.

## EMPTY VALUES

To set a key to an empty value, `""` should be passed for the `value` argument.

## OPTIONS

- `-H`, `--help`:
  Print help text.
- `-V`, `--version`:
  Print version info.
- `-v`, `--verbose`:
  Explain what is happening.
- `-N`, `--namespace ns`:
  Specify the namespace to use when writing cascading keys
  Default: value of /sw/kdb/current/namespace or user.

## EXAMPLES

To set a Key to the value `Hello World!`:  
	`kdb set user/example/key "Hello World!"`  

To create a new key with a null value:  
	`kdb set user/example/key`  

To set a key to an empty value:
	`kdb set user/example/key ""`

## SEE ALSO

- For difference between empty and null values, see [elektra-values(7)](elektra-values.md)
