# kdb-record-state(1) -- Print information about the state of a recording session

## SYNOPSIS

`kdb record-state`<br>

## DESCRIPTION

This command prints out information about the current state of the recording session.
This includes:

- Whether recording is currently enabled or not
- What parts of the KDB will be recorded
- How many keys have been recorded
- Which keys have been recorded

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
