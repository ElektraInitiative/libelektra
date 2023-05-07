# kdb-record-reset(1) -- Reset the recording session

## SYNOPSIS

`kdb record-reset`<br>

## DESCRIPTION

This command removes all the recorded changes in the KDB.
It will NOT undo the changes themselves.
This command can be executed while recording is active and when it is not active.

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
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## RETURN VALUE

- 0:
  Successful.
- 1-10:
  standard exit codes, see [kdb(1)](kdb.md).
- 11:
  An error occurred during reset.

## SEE ALSO

- [kdb-record-start(1)](kdb-record-start.md) on how to start the recording session
- [kdb-record-stop(1)](kdb-record-stop.md) on how to stop the recording session
- [kdb-record-state(1)](kdb-record-state.md) on how to get information about the current recording session
- [kdb-record-undo(1)](kdb-record-undo.md) on how to undo changes that have been recorded
- [kdb-record-rm(1)](kdb-record-rm.md) on how to remove a single key from the recording session
