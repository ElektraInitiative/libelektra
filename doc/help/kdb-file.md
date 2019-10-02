# kdb-file(1) -- Displays which file a key is stored in

## SYNOPSIS

`kdb file <key name>`<br>

Where `key name` is the name of the key to check.<br>

## DESCRIPTION

This command prints which file a given key is stored in.<br>
While many keys are stored in a default key database file, many others are stored in any number of configuration files located all over the system.<br>
This tool is made to allow users to find out the file that a key is actually stored in.<br>
This command makes use of Elektra’s `resolver` plugin which the uer can learn more about by running the command `kdb plugin-info resolver`.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-n`, `--no-newline`:
  Suppress the newline at the end of the output.
- `-N`, `--namespace <namespace>`:
  Specify the namespace to use when writing cascading keys.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## KDB

- `/sw/elektra/kdb/#0/current/namespace`:
  Specifies which default namespace should be used when setting a cascading name.
  By default it is `user`, except if you are root, then it is `system`.

## EXAMPLES

To find which file a key is stored in:<br>
`kdb file user/example/key`<br>

## SEE ALSO

- [elektra-mounting(7)](elektra-mounting.md)
- [elektra-namespaces(7)](elektra-namespaces.md)
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
