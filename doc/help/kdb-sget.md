# kdb-sget(1) -- Get the value of a key stored in the key database from a script

## SYNOPSIS

`kdb sget <key name> <default-value>`

Where `key name` is the name of the key to retrieve and `default-value` is the value that should be printed if no value can be retrieved.

## DESCRIPTION

This command is used to retrieve the value of a key from within a script.
When using the kdb tool in a script, the user should use the `sget` command in place of the kdb-get(1) command.
The kdb-get(1) command should not be used in scripts because it may return an error instead of printing a value in certain circumstances.
The `sget` command guarantees that a value will be printed (unless the user passes faulty arguments).
This command will either print the value of the key it retrieves or a default value that the user specifies.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.

## EXAMPLES

To get the value of a key from a script or return the value `0`:<br>
`kdb sget user:/example/key 0`

To get the value of a key using a cascading lookup or return the value `notfound`:<br>
`kdb sget /example/key "notfound"`

## SEE ALSO

- [kdb-get(1)](kdb-get.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
