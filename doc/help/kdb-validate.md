# kdb-validate(1) - Validate key values

## SYNOPSIS

`kdb validate`

## DESCRIPTION

Validate the values of string keys below a given name using the loaded validation plugins (eg. range or validation) by reading all values, making them dirty by changing to another value, changing back to original and then writing that back to the key database.

This command is useful for validating configuration files against
their specifications.

For keys to be validated, they must contain the 'check'-metakeys
and the respective plugins for validation must be loaded
for the backend that was used while mounting.
If a validation is done while using `kdb set` or `kdb get`
the same validation is also done by `kdb validate`
Only string keys are validated! Binary keys are skipped!

Use `-f` to do a write-test even if the previous read
from the key database has issued warnings.

## OPTIONS

- `-d`,`--debug`:
  Give debug information.
- `-f`, `--force`:
  Force writing the configuration even on warnings.
- `-H`, `--help`:
  Show usage of command.
- `-p <name>`, `--profile <name>`:
  Use a different profile for kdb configuration.
- `-v`, `--verbose`:
  Explain what is happening.
- `-V`, `--version`:
  Print version info.
- `-C <when>`, `--color <when>`:
  Print `never/auto(default)/always` colored output.

## RETURN VALUES

This command will return the following values as an exit status:<br>

- 0:
  Validated correctly.
- 11:
  Could not get the values.
- 12:
  Error occurred while writing back the values.
