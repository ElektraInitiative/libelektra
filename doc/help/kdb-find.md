kdb-find(1) -- Find keys in the key database
================================

## SYNOPSIS

`kdb find <regex>`

Where `regex` is a regular expression which contains the key to find.

## DESCRIPTION

This command will list the name of all keys that contain `regex`.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Explain what is happening.
- `-0`, `--null`:
  Use binary 0 termination.

## EXAMPLES

```sh
# Backup-and-Restore: /tests

# Create the keys we use for the examples
kdb set /tests/tests val1
kdb set /tests/tests/foo/bar val2
kdb set /tests/tests/fizz/buzz fizzbuzz
kdb set /tests/tostfizz val3
kdb set /tests/tust/level lvl

# list all keys containing /tests/t[eo]
kdb find '/tests/t[eo]'
#> user/tests/tests
#> user/tests/tests/fizz/buzz
#> user/tests/tests/foo/fizzbar
#> user/tests/tostfizz

# list all keys containing fizz
kdb find 'fizz'
#> user/tests/tests/fizz/buzz
#> user/tests/tests/foo/fizzbar
#> user/tests/tostfizz
```

## SEE ALSO

- If the user would also like to see the values of the keys below `path` then you should
consider the [kdb-export(1)](kdb-export.md) command.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
