# kdb-plugin-list(1) -- List plugins available to Elektra

## SYNOPSIS

`kdb plugin-list [provider]`

## DESCRIPTION

This command will either list all available Elektra plugins
or all plugins that provide a specific functionality.

The output will be sorted by their status.
The best plugins will be the last in the list.

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
  Also output the number calculated by their
  `infos/status` clause in the contract. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.
- `-0`, `--null`:
  Use binary 0 termination

## EXAMPLES

To get a sorted list of all available plugins with their status:<br>
`kdb plugin-list -v`

To get a sorted list of all storage plugins:<br>
`kdb plugin-list storage`

To get a sorted list of all plugins that provide `ini` with their status:<br>
`kdb plugin-list -v ini`

## SEE ALSO

- `infos/status` is part of the [elektra-contracts.md (7)](elektra-contracts.md)
