# kdb-basename(1) -- Get the basename of a key name

## SYNOPSIS

`kdb basename <key name>`

Where `key name` is the name of the key.

## DESCRIPTION

This command is used to retrieve the basename of a key.

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
kdb basename user:/key/subkey
#> subkey

kdb basename user:/key/subkey/
#> subkey

kdb basename /
#>
```

## SEE ALSO

- See [kdb-namespace(1)](kdb-namespace.md) and [kdb-dirname(1)](kdb-dirname.md) for other namepart extraction operations.
- The Unix analogue [BASENAME(1)].
- To get keys in shell scripts, you can use [kdb-sget(1)](kdb-sget.md).
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
