# kdb-dirname(1) -- Get the cascading directory name of a key name

## SYNOPSIS

`kdb dirname <key name>`

Where `key name` is the name of the key.

## DESCRIPTION

This command is used to retrieve the directory name of a key, without any namespace.

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
kdb dirname user:/key/subkey
#> /key

kdb dirname /
#> /
```

## SEE ALSO

- Use [kdb-dirkey(1)](kdb-dirkey.md) to obtain the directory of a key _with namespace included_.
- See [kdb-namespace(1)](kdb-namespace.md), [kdb-basename(1)](kdb-basename.md) for other namepart extraction operations.
- The Unix analogue [DIRNAME(1)].
- To get keys in shell scripts, you can use [kdb-sget(1)](kdb-sget.md).
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
