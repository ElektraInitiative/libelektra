kdb-setmeta(1) -- Set the value of a meta key
=============================================

## SYNOPSIS

`kdb setmeta <key-name> <meta-name> <meta-value>`  

Where `key-name` is the path to the key that the meta key is associated with,
`meta-name` is the name of the meta key the user would like to set the value of (or create),
and `meta-value` is the value the user wishes to set the meta key to.

## DESCRIPTION

This command allows the user to set the value of an individual meta key.
If a key does not already exist and the user tries setting a meta key associated with it, the key will be created with a null value.
There is some special handling for the meta data atime, mtime and ctime. They will be converted to time_t.

For cascading keys, the namespace will default to `spec`, because
that is the place where you usually want to set meta data.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-v`, `--verbose`:
  Explain what is happening.


## EXAMPLES

To set a meta key called `description` associated with the key `user/example/key` to the value `Hello World!`:  
	`kdb setmeta spec/example/key description "Hello World!"`  

To create a new key `spec/example/newkey` with a null value (if it did not exist before)
and a meta key `namespace/#0` associated with it to the value `system`:  
	`kdb setmeta /example/newkey "namespace/#0" system`

To create an override link for a `/test` key:  
	`kdb set /overrides/test "example override"`  
	`sudo kdb setmeta spec/test override/#0 /overrides/test`

## SEE ALSO

- How to get meta data: [kdb-getmeta(1)](kdb-getmeta.md)
- [elektra-meta-data(7)](elektra-meta-data.md)
