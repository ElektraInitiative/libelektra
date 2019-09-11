# kdb-run-all(1) -- run all tests

## SYNOPSIS

`kdb run_all`

## DESCRIPTION

This command runs all tests. By default the tool only prints the output of failed tests.

## OPTIONS

- `-v`:
  Print the full output of each test.

## ENVIRONMENT

- `KDB`:
  Use a different kdb command instead of `kdb`
- `CHECK_VERSION`
  Ignore version checks, e.g. allow old unit tests to
  run against newer Elektra library
