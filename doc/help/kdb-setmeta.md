kdb-setmeta(1) -- Set the value of a meta key
=============================================

## SYNOPSIS

`kdb setmeta <path> <meta-name> <meta-value>`  

Where `path` is the path to the key that the meta key is associated with,
`meta-name` is the name of the meta key the user would like to set the value of (or create),
and `meta-value` is the value the user wishes to set the meta key to.  

## DESCRIPTION

This command allows the user to set the value of an individual meta key.  
If a key does not already exist and the user tries setting a meta key associated with it, the key will be created with a null value.  
There is some special handling for the metadata atime, mtime and ctime. They will be converted to time_t.  

## OPTIONS

- `-H`, `--help`:
  Print help text.
- `-V`, `--version`:
  Print version info.
- `-v`, `--verbose`:
  Explain what is happening.


## EXAMPLES

To set a meta key called `message` associated with the key `user/example/key` to the value `Hello World!`:  
	`kdb setmeta user/example/key message "Hello World!"`  

To create a new key with a null value and a meta key associated with it to the value `10`:  
	`kdb setmeta user/example/key metakey 10`  


## SEE ALSO

- [elektra-meta-data(7)](elektra-meta-data.md)
