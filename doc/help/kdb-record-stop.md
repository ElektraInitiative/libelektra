# kdb-record-stop(1) -- Stop session recording

## SYNOPSIS

`kdb record-stop`<br>

## DESCRIPTION

This command stops the current recording session.
Changes that are made to KDB after this command will no longer be recorded.
The recording session will NOT be cleared.

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
- [kdb-record-state(1)](kdb-record-state.md) on how to get information about the current recording session
- [kdb-record-clear(1)](kdb-record-clear.md) on how to clear the recording session
