# kdb-record-state(1) -- Print information about the state of a recording session

## SYNOPSIS

`kdb record-state`<br>

## DESCRIPTION

This command prints out information about the current state of the recording session.
This includes:

- Whether recording is currently enabled or not
- What parts of the KDB will be recorded in the future.
  (The session may already contain changes from other parts of the KDB)
- How many keys have been recorded
- Which keys have been recorded

For now, the output format is not considered stable and may change over time.

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
  standard exit codes, see [kdb(1)](kdb.md)
- 11:
  An error occurred during gathering the state information.

## EXAMPLES

Here is an example output of `kdb record-state`:

```
Recording is active for user:/

Added 1 key(s)
Modified 1 key(s)
Removed 1 key(s)

Added key user:/test/age
Modified key user:/test/name
Removed key user:/test/color
```

## SEE ALSO

- [kdb-record-start(1)](kdb-record-start.md) on how to start the recording session
- [kdb-record-stop(1)](kdb-record-stop.md) on how to stop the recording session
