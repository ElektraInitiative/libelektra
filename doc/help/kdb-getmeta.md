kdb-getmeta(1) -- Get the value of a meta key stored in the key database
========================================================================

## SYNOPSIS

`kdb getmeta <key-name> <meta-name>`  

Where `key-name` is the full path to the key and
`meta-name` is the name of the meta key the user would like to access.

## DESCRIPTION

This command is used to print the value of a meta key.
A meta key is information stored in a key which describes that key.

The handling of cascading `key-name` does not differ to `kdb get`.
Make sure to use the namespace `spec`, if you want meta-data from there.

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
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-n`, `--no-newline`:
  Suppress the newline at the end of the output.

## EXAMPLES

To get the value of a meta key called `description` stored in the key `spec/example/key`:  
`kdb getmeta spec/example/key description`

To get the value of meta key called `override/#0` stored in the key `spec/example/dir/key`:  
`kdb getmeta spec/example/dir/key "override/#0"`

## SEE ALSO

- How to set meta data: [kdb-setmeta(1)](kdb-setmeta.md)
- For more about cascading keys see [elektra-cascading(7)](elektra-cascading.md)
- For general information about meta data see [elektra-meta-data(7)](elektra-meta-data.md)
