# kdb-complete(1) -- Show suggestions how to complete a given path

## SYNOPSIS

`kdb complete [path]`

Where `path` is the path for which the user would like to receive completion suggestion.
Calling `kdb complete ""` will show every possible completion.

## DESCRIPTION

Show suggestions how the current name could be completed.
Suggestions will include existing key names, path segments of existing key names,
namespaces and mount points.
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
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-m`, `--min-depth <min-depth>`:
  Specify the minimum depth of completion suggestions (0 by default), exclusive
  and relative to the name to complete.
- `-M`, `--max-depth <max-depth>`:
  Specify the maximum depth of completion suggestions (unlimited by default, 1
  to show only the next level), inclusive and relative to the name to complete.
- `-v`, `--verbose`:
  Give a more detailed output, showing the number of child nodes and the depth level. Prints additional information in case of errors/warnings.
- `-0`, `--null`:
  Use binary 0 termination.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## EXAMPLES

```sh
# Backup-and-Restore: user:/tests/complete/examples

# Create the keys we use for the examples
kdb set user:/tests/complete/examples/kdb-complete/level1 foo
kdb set user:/tests/complete/examples/kdb-complete/lvl1/lvl2 bar
kdb set user:/tests/complete/examples/kdb-complete/lvl1/lvl2/lvl3/lvl4/lvl5 fizz
kdb set user:/tests/complete/examples/kdb-complete/buzz fizzBuzz
kdb set user:/tests/complete/examples/kdb-complete/#1 asdf
kdb set user:/tests/complete/examples/kdb-complete/% nothing

# list suggestions for namespaces starting with us, only the current level
kdb complete us --max-depth=1
#> user:/

# list suggestions for namespaces starting with user, only the current level
kdb complete user --max-depth=1
#> user:/

# list suggestions for the namespace user, only the next level as it ends with /
# note the difference to the previous example, which uses no trailing /
kdb complete user:/ --max-depth=1
# STDOUT-REGEX: .+

# list all possible namespaces or mount points, only the current level
kdb complete "" --max-depth=1
# STDOUT-REGEX: .+

# list suggestions for /tests/complete/examples/kdb-complete, only the current level
kdb complete /tests/complete/examples/kdb-complete --max-depth=1
#> user:/tests/complete/examples/kdb-complete/

# list suggestions for /tests/complete/examples/kdb-complete/, only the next level
# again, note the difference to the previous example which has no trailing /
kdb complete /tests/complete/examples/kdb-complete/ --max-depth=1
#> user:/tests/complete/examples/kdb-complete/%
#> user:/tests/complete/examples/kdb-complete/#1
#> user:/tests/complete/examples/kdb-complete/buzz
#> user:/tests/complete/examples/kdb-complete/level1
#> user:/tests/complete/examples/kdb-complete/lvl1/

# list suggestions for /tests/complete/examples/kdb-complete which are minimum 2 levels
# away from that key, and maximum 4 levels away
kdb complete /tests/complete/examples/kdb-complete/ --min-depth=2 --max-depth=4
#> user:/tests/complete/examples/kdb-complete/lvl1/lvl2/lvl3/
#> user:/tests/complete/examples/kdb-complete/lvl1/lvl2/lvl3/lvl4/

kdb rm -r user:/tests/complete/examples/kdb-complete
```

## SEE ALSO

- If you would only like to see existing keys under a given path, consider using
  the [kdb-ls(1)](kdb-ls.md) command.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
