# kdb-record-rm(1) -- Remove a key from the recording session

## SYNOPSIS

`kdb record-rm <key>`<br>

## DESCRIPTION

This command removes the specified `key` from the recording session.

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

## SEE ALSO

- [kdb-record-start(1)](kdb-record-start.md) on how to start the recording session
- [kdb-record-stop(1)](kdb-record-stop.md) on how to stop the recording session
- [kdb-record-state(1)](kdb-record-state.md) on how to get information about the current recording session
- [kdb-record-clear(1)](kdb-record-clear.md) on how to remove all keys from the recording session
- [kdb-record-undo(1)](kdb-record-undo.md) on how to undo changes that have been recorded