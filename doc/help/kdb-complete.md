kdb-complete(1) -- Show suggestions how to complete a given path
================================

## SYNOPSIS

`kdb complete [path]`  

Where `path` is the path for which the user would like to receive completion suggestion.
If `path` is not specified, it will show every possible completion. Its synonymous 
to calling `kdb complete ""`.

## DESCRIPTION

Show suggestions how the current name could be completed.
Suggestions will include existing key names, path segments of existing key names, 
namespaces and mountpoints.
Additionally, the output will indicate whether the given path is a node or a leaf 
in the hierarchy of keys, nodes end with '/' as opposed to leaves.
It will also work for cascading keys, and will additionally display a cascading 
key's namespace in the output to indicate from which namespace this suggestion 
originates from.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-m`, `--min-depth`=<min-depth>:
  Specify the minimum depth of completion suggestions (0 by default), exclusive 
  and relative to the name to complete.
- `-M`, `--max-depth`=<max-depth>:
  Specify the maximum depth of completion suggestions (unlimited by default, 1 
  to show only the next level), inclusive and relative to the name to complete.
- `-v`, `--verbose`:
  Give a more detailed output, showing the number of child nodes and the depth level.
- `-0`, `--null`:
  Use binary 0 termination.
- `-d`, `--debug`:
  Give debug information.

## EXAMPLES

```sh

# Create the keys we use for the examples
kdb set /sw/elektra/examples/kdb-complete/level1 foo
kdb set /sw/elektra/examples/kdb-complete/lvl1/lvl2 bar
kdb set /sw/elektra/examples/kdb-complete/lvl1/lvl2/lvl3/lvl4/lvl5 fizz
kdb set /sw/elektra/examples/kdb-complete/buzz fizzBuzz
kdb set /sw/elektra/examples/kdb-complete/#array_1 asdf
kdb set /sw/elektra/examples/kdb-complete/% nothing

# list suggestions for namespaces starting with us, only the current level
kdb complete us --max-depth=1
#>user/

# list suggestions for namespaces starting with user, only the current level
kdb complete user --max-depth=1
#>user/

# list suggestions for the namespace user, only the next level as it ends with /
# note the difference to the previous example, which uses no trailing /
kdb complete user/ --max-depth=1
#>RET: 0
#>STDOUT: .*

# list all possible namespaces or mountpoints, only the current level
kdb complete --max-depth=1
#>RET: 0
#>STDOUT: .*

# list suggestions for /sw/elektra/examples/kdb-complete, only the current level
kdb complete /sw/elektra/examples/kdb-complete --max-depth=1
#>user/sw/elektra/examples/kdb-complete/

# list suggestions for /sw/elektra/examples/kdb-complete/, only the next level
# again, note the difference to the previous example which has no trailing /
kdb complete /sw/elektra/examples/kdb-complete/ --max-depth=1
#>user/sw/elektra/examples/kdb-complete/%
#>user/sw/elektra/examples/kdb-complete/#array_1
#>user/sw/elektra/examples/kdb-complete/buzz
#>user/sw/elektra/examples/kdb-complete/level1
#>user/sw/elektra/examples/kdb-complete/lvl1/

# list suggestions for /sw/elektra/examples/kdb-complete which are minimum 2 levels
# away from that key, and maximum 4 levels away
kdb complete /sw/elektra/examples/kdb-complete/ --min-depth=2 --max-depth=4
#>user/sw/elektra/examples/kdb-complete/lvl1/lvl2/lvl3/
#>user/sw/elektra/examples/kdb-complete/lvl1/lvl2/lvl3/lvl4/

```

## SEE ALSO

- If you would only like to see existing keys under a given path, consider using 
  the [kdb-ls(1)](kdb-ls.md) command.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
