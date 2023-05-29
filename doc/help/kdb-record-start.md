# kdb-record-start(1) -- Start session recording

## SYNOPSIS

`kdb record-start [<parent_key>]`<br>

## DESCRIPTION

This command starts session recording.
If there is already a previous session, this only changes the parent key.
If you want to make sure you start at a clean state, use `kdb record-reset` first.
By default, all changes to the KDB are recorded.
The optional parameter `parent_key` can be used to restrict recording to a specific subtree of the KDB.

Note: when you activate session recording, concurrency of Elektra will be somewhat limited.
As long as it is active, a global lock will be created to ensure no two processes will write data simultaneously.
This behavior is similar as to when multiple processes will write to the same configuration file.
Applications should already handle this case gracefully, and just retry writing their configuration.

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
  An error occurred during starting the recording.

## SEE ALSO

- [kdb-record-stop(1)](kdb-record-stop.md) on how to stop the recording session
- [kdb-record-state(1)](kdb-record-state.md) on how to get information about the current recording session
- [kdb-record-reset(1)](kdb-record-reset.md) on how to clear the recording session
