# kdb-record-export(1) -- Export recorded changes

## SYNOPSIS

`kdb record-export <source>`<br>

## DESCRIPTION

This command exports the recorded changes into the specified output format.
Keys are exported to `stdout` in whichever format is specified.

## USAGE

Where `source` is the path of the key(s) you want to export.
Additionally, the user can specify a format to use by passing it as the option argument `format`.
The `format` attribute relies on Elektra’s plugin system to export the keys in the desired format.
The parameter `format` must be the name of a valid storage plugin.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-F`, `--format`:
  The format to use.
- `-E`, `--without-elektra`:
  Omit the `system:/elektra` directory.
- `-S`, `--include-recording-session`:
  Include the recording session in the output.
- `-c`, `--plugins-config <plugins-config>`:
  Add a configuration to the format plugin.
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
  An error occurred during exporting.

## SEE ALSO

- [kdb-record-start(1)](kdb-record-start.md) on how to start the recording session
- [kdb-record-stop(1)](kdb-record-stop.md) on how to stop the recording session
- [kdb-record-state(1)](kdb-record-state.md) on how to get information about the current recording session
