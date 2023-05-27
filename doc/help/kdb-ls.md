# kdb-ls(1) -- List keys in the key database

## SYNOPSIS

`kdb ls <path>`

Where `path` is the path in which the user would like to list keys below.

## DESCRIPTION

This command will list the name of all keys below a given path.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-m`, `--min-depth <min-depth>`:
  Specify the minimum path depth of the output (0 by default), exclusive
  and relative to the name to list.
- `-M`, `--max-depth <max-depth>`:
  Specify the maximum path depth of the output (unlimited by default, 1
  to show only the next level), inclusive and relative to the name to list.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.
- `-0`, `--null`:
  Use binary 0 termination.

## EXAMPLES

```sh
# Backup-and-Restore: user:/tests/examples

# We use `dump` as storage format here, since storage plugins such as INI
# automatically add keys between levels (e.g. `user:/tests/examples/kdb-ls/test/foo`).
sudo kdb mount ls.ecf user:/tests/examples dump

# Create the keys we use for the examples
kdb set user:/tests/examples/kdb-ls/test val1
kdb set user:/tests/examples/kdb-ls/test/foo/bar val2
kdb set user:/tests/examples/kdb-ls/test/fizz/buzz fizzbuzz
kdb set user:/tests/examples/kdb-ls/tost val3
kdb set user:/tests/examples/kdb-ls/tost/level lvl

# list all keys below /tests/examples/kdb-ls
kdb ls /tests/examples/kdb-ls
#> user:/tests/examples/kdb-ls/test
#> user:/tests/examples/kdb-ls/test/fizz/buzz
#> user:/tests/examples/kdb-ls/test/foo/bar
#> user:/tests/examples/kdb-ls/tost
#> user:/tests/examples/kdb-ls/tost/level

# list the next level of keys below /tests/examples/kdb-ls
# note that if the search key ends with a /, it lists the next level
kdb ls /tests/examples/kdb-ls/ --max-depth=1
#> user:/tests/examples/kdb-ls/test
#> user:/tests/examples/kdb-ls/tost

# list the current level of keys below /tests/examples/kdb-ls
# note the difference to the previous example
kdb ls /tests/examples/kdb-ls --max-depth=1
# this yields no output as /tests/examples/kdb-ls is not a key

# list all keys below /tests/examples/kdb-ls with are minimum 1 level (inclusive) away from that key
# and maximum 2 levels away (exclusive)
kdb ls /tests/examples/kdb-ls --min-depth=1 --max-depth=2
#> user:/tests/examples/kdb-ls/test
#> user:/tests/examples/kdb-ls/tost

# list all keys below /tests/examples/kdb-ls/test
kdb ls /tests/examples/kdb-ls/test
#> user:/tests/examples/kdb-ls/test
#> user:/tests/examples/kdb-ls/test/fizz/buzz
#> user:/tests/examples/kdb-ls/test/foo/bar

# list all keys under /tests/examples/kdb-ls in verbose mode
kdb ls /tests/examples/kdb-ls/ -v
#> size of all keys in mount point: 5
#> size of requested keys: 5
#> user:/tests/examples/kdb-ls/test
#> user:/tests/examples/kdb-ls/test/fizz/buzz
#> user:/tests/examples/kdb-ls/test/foo/bar
#> user:/tests/examples/kdb-ls/tost
#> user:/tests/examples/kdb-ls/tost/level

kdb rm -r user:/tests/examples
sudo kdb umount user:/tests/examples
```

## SEE ALSO

- If the user would also like to see the values of the keys below `path` then you should
  consider the [kdb-export(1)](kdb-export.md) command.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
