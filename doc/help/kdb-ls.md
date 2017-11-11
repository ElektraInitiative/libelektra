kdb-ls(1) -- List keys in the key database
================================

## SYNOPSIS

`kdb ls <path>`

Where `path` is the path in which the user would like to list keys below.

## DESCRIPTION

This command will list the name of all keys below a given path.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-m`, `--min-depth <min-depth>`:
  Specify the minimum path depth of the output (0 by default), exclusive
  and relative to the name to list.
- `-M`, `--max-depth <max-depth>`:
  Specify the maximum path depth of the output (unlimited by default, 1
  to show only the next level), inclusive and relative to the name to list.
- `-v`, `--verbose`:
  Explain what is happening.
- `-0`, `--null`:
  Use binary 0 termination.
- `-d`, `--debug`:
  Give debug information.

## EXAMPLES

```sh

# Create the keys we use for the examples
kdb set /sw/elektra/examples/kdb-ls/test val1
kdb set /sw/elektra/examples/kdb-ls/test/foo/bar val2
kdb set /sw/elektra/examples/kdb-ls/test/fizz/buzz fizzbuzz
kdb set /sw/elektra/examples/kdb-ls/tost val3
kdb set /sw/elektra/examples/kdb-ls/tost/level lvl

# list all keys below /sw/elektra/examples/kdb-ls
kdb ls /sw/elektra/examples/kdb-ls
#>user/sw/elektra/examples/kdb-ls/test
#>user/sw/elektra/examples/kdb-ls/test/fizz/buzz
#>user/sw/elektra/examples/kdb-ls/test/foo/bar
#>user/sw/elektra/examples/kdb-ls/tost
#>user/sw/elektra/examples/kdb-ls/tost/level

# list the next level of keys below /sw/elektra/examples/kdb-ls
# note that if the search key ends with a /, it lists the next level
kdb ls /sw/elektra/examples/kdb-ls/ --max-depth=1
#>user/sw/elektra/examples/kdb-ls/test
#>user/sw/elektra/examples/kdb-ls/tost

# list the current level of keys below /sw/elektra/examples/kdb-ls
# note the difference to the previous example
kdb ls /sw/elektra/examples/kdb-ls --max-depth=1
# this yields no output as /sw/elektra/examples/kdb-ls is not a key

# list all keys below /sw/elektra/examples/kdb-ls with are minimum 1 level away from that key
# and maximum 2 levels away
kdb ls /sw/elektra/examples/kdb-ls --min-depth=1 --max-depth=2
#>user/sw/elektra/examples/kdb-ls/tost/level

# list all keys below /sw/elektra/examples/kdb-ls/test
kdb ls /sw/elektra/examples/kdb-ls/test
#>user/sw/elektra/examples/kdb-ls/test
#>user/sw/elektra/examples/kdb-ls/test/fizz/buzz
#>user/sw/elektra/examples/kdb-ls/test/foo/bar

# list all keys under /sw/elektra/examples/kdb-ls in verbose mode
kdb ls /sw/elektra/examples/kdb-ls/ -v
#>size of all keys in mountpoint: 31
#>size of requested keys: 5
#>user/sw/elektra/examples/kdb-ls/test
#>user/sw/elektra/examples/kdb-ls/test/fizz/buzz
#>user/sw/elektra/examples/kdb-ls/test/foo/bar
#>user/sw/elektra/examples/kdb-ls/tost
#>user/sw/elektra/examples/kdb-ls/tost/level

```

## SEE ALSO

- If the user would also like to see the values of the keys below `path` then you should
consider the [kdb-export(1)](kdb-export.md) command.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
