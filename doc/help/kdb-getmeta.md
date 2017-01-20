kdb-getmeta(1) -- Get the value of a metakey stored in the key database
========================================================================

## SYNOPSIS

`kdb getmeta <key name> <metaname>`  

Where `key name` is the name of the key and
`metaname` is the name of the metakey the user would like to access.

## DESCRIPTION

This command is used to print the value of a metakey.
A metakey is information stored in a key which describes that key.

The handling of cascading `key name` does not differ to `kdb get`.
Make sure to use the namespace `spec`, if you want metadata from there.

## RETURN VALUES

This command will return the following values as an exit status:  
* 0:
  No errors.
* 1:
  Key not found. (Invalid `path`)
* 2:
  Meta key not found. (Invalid `metaname`).


## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-n`, `--no-newline`:
  Suppress the newline at the end of the output.
- `-C`, `--color`=[when]:
  Print never/auto(default)/always colored output.

## EXAMPLES

To get the value of a metakey called `description` stored in the key `spec/example/key`:  
`kdb getmeta spec/example/key description`

To get the value of metakey called `override/#0` stored in the key `spec/example/dir/key`:  
`kdb getmeta spec/example/dir/key "override/#0"`

## SEE ALSO

- How to set metadata: [kdb-setmeta(1)](kdb-setmeta.md)
- For more about cascading keys see [elektra-cascading(7)](elektra-cascading.md)
- For general information about metadata see [elektra-metadata(7)](elektra-metadata.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
