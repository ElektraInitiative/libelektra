kdb-test(1) -- Run test(s) on the key database
==============================================

## SYNOPSIS

`kdb test <path> [<test-name> ...]`  

Where `path` is the path the user wishes to perform the test under.
The option `test-name` argument is used to specify which test(s) to run. To run multiple tests, each should be named with a trailing space.  
If no `test-name` is provided, all the tests will be run.  

## DESCRIPTION

This command is used to run part or all of the key database test suite.  
These tests allow one to user to verify that a backend is capable of storing and retrieving all kinds of configuration keys and values.  

The following tests are available: basic string umlauts binary naming meta  

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.

## EXAMPLES

To run all tests below the `user/example/tests` key:  
`kdb test user/example/tests`  

To run the `binary` and `naming` tests:  
`kdb test user/example/tests binary naming`  

## SEE ALSO

- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
