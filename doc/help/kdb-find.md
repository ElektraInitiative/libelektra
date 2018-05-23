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
# Backup-and-Restore: /test

# Create the keys we use for the examples
kdb set /test/test val1
kdb set /test/test/foo/bar val2
kdb set /test/test/fizz/buzz fizzbuzz
kdb set /test/tostfizz val3
kdb set /test/tust/level lvl

# list all keys containing /test/t[eo]
kdb find '/test/t[eo]'
#> user/test/test
#> user/test/test/fizz/buzz
#> user/test/test/foo/fizzbar
#> user/test/tostfizz

# list all keys containing fizz
kdb find 'fizz'
#> user/test/test/fizz/buzz
#> user/test/test/foo/fizzbar
#> user/test/tostfizz
```

## SEE ALSO

- If the user would also like to see the values of the keys below `path` then you should
consider the [kdb-export(1)](kdb-export.md) command.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
