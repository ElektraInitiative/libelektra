# kdb-record-undo(1) -- Undo the changes performed during a recording session

## SYNOPSIS

`kdb record-undo`<br>

## DESCRIPTION

This command will undo all the changes that have been recorded.
After execution, the recording session will no longer contain the undone keys.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-P`, `--parent <key>`:
  Restrict the undo operation to a specific subtree of the KDB.
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
  standard exit codes, see [kdb(1)](kdb.md)
- 11:
  An error occurred during undoing the recorded changes.

## SEE ALSO

- [kdb-record-start(1)](kdb-record-start.md) on how to start the recording session
- [kdb-record-state(1)](kdb-record-state.md) on how to get information about the current recording session
- [kdb-record-stop(1)](kdb-record-stop.md) on how to stop the recording session
