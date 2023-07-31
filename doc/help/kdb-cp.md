# kdb-cp(1) -- Copy keys within the key database

## SYNOPSIS

`kdb cp <source> <dest>`

## DESCRIPTION

This command copies key(s) in the Key database.
You can copy keys to another directory within the database or even below another key.
Note that you cannot copy a key below itself.

The argument `source` is the path of the key(s) you want to copy and `dest` is the path where you would like to copy the key(s) to.
Note that when using the `-r` flag, `source` as well as all the keys below will be copied.

## LIMITATIONS

Neither `source` nor `dest` can be a cascading key.
(Start with `/`).
Make sure to select a namespace.

## RETURN VALUES

This command will return the following values as an exit status:

- 0:
  No errors.
- 1-10:
  standard exit codes, see [kdb(1)](kdb.md)
- 11:
  No key to copy found.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-r`, `--recursive`:
  Recursively copy keys.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.
- `-f`, `--force`:
  Force overwriting the keys.

## EXAMPLES

```sh
# Backup-and-Restore: user:/tests/cp/examples

# Create the keys we use for the examples
kdb set user:/tests/cp/examples/kdb-cp/key key1
kdb set user:/tests/cp/examples/kdb-cp/key/first key
kdb set user:/tests/cp/examples/kdb-cp/key/second key
kdb set user:/tests/cp/examples/kdb-cp/cpkey key1
kdb set user:/tests/cp/examples/kdb-cp/cpkey/first key
kdb set user:/tests/cp/examples/kdb-cp/cpkey/second key
kdb set user:/tests/cp/examples/kdb-cp/cpkeyerror/first key
kdb set user:/tests/cp/examples/kdb-cp/cpkeyerror/second anotherValue
kdb set user:/tests/cp/examples/kdb-cp/another/key one
kdb set user:/tests/cp/examples/kdb-cp/another/value two

# To copy a single key:
kdb cp user:/tests/cp/examples/kdb-cp/key user:/tests/cp/examples/kdb-cp/key2
#>

# To copy multiple keys:
kdb cp -r user:/tests/cp/examples/kdb-cp/key user:/tests/cp/examples/kdb-cp/copied
#>

# If the target-key already exists and has a different value, cp fails:
kdb cp -r user:/tests/cp/examples/kdb-cp/key user:/tests/cp/examples/kdb-cp/cpkeyerror
# RET: 11

# If the target-key already exists and has the same value as the source, everything is fine:
kdb cp -r user:/tests/cp/examples/kdb-cp/key user:/tests/cp/examples/kdb-cp/cpkey
#>

# To force the copy of keys:
kdb cp -rf user:/tests/cp/examples/kdb-cp/key user:/tests/cp/examples/kdb-cp/cpkeyerror
#>

# Now the key-values of /cpkeyerror are overwritten:
kdb export user:/tests/cp/examples/kdb-cp/cpkeyerror mini
#> =key1
#> first=key
#> second=key

# To copy keys below an existing key:
kdb cp -r user:/tests/cp/examples/kdb-cp/another user:/tests/cp/examples/kdb-cp/another/key
#>

kdb rm -r user:/tests/cp/examples/kdb-cp/
```
