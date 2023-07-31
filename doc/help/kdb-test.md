# kdb-test(1) -- Run test(s) on the key database

## SYNOPSIS

`kdb test <path> [<test-name> ...]`<br>

Where `path` is the path the user wishes to perform the test under.
The option `test-name` argument is used to specify which test(s) to run. To run multiple tests, each should be named with a trailing space.<br>
If no `test-name` is provided, all the tests will be run.<br>

## DESCRIPTION

This command is used to run part or all of the key database test suite.<br>
These tests allow one to user to verify that a backend is capable of storing and retrieving all kinds of configuration keys and values.<br>

The following tests are available: basic string umlauts binary naming meta<br>

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.

## EXAMPLES

To run all tests below the `user:/tests/example` key:<br>
`kdb test user:/tests/example`<br>

To run the `binary` and `naming` tests:<br>

```sh
kdb test user:/tests/example binary naming
# RET: 0

# clean-up
kdb rm -r user:/tests/example
```

## SEE ALSO

- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
