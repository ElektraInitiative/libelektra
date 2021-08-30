# kdb-namespace(1) -- Get the namespace of a key name

## SYNOPSIS

`kdb namespace <key name>`

Where `key name` is the name of the key.

## DESCRIPTION

This command is used to retrieve the namespace of a key, including the eventual trailing ':'.
For the cascading namespace, the empty string is returned.

## RETURN VALUES

This command will return the following values as an exit status:

- 0:
  No errors.
- 7:
  invalid key name passed, see [kdb(1)](kdb.md) for a list of return codes used by kdb.

## OPTIONS

- `-n`, `--no-newline`:
  Suppress the newline at the end of the output.

## EXAMPLES

```sh
kdb namespace user:/key/subkey
#> user:

kdb namespace /key/subkey
#> 
```

## SEE ALSO

- See [kdb-basename(1)](kdb-basename.md), [kdb-dirname(1)](kdb-dirname.md), [kdb-dirkey(1)](kdb-dirkey.md) for other namepart extraction operations.
- To get keys in shell scripts, you can use [kdb-sget(1)](kdb-sget.md).
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
