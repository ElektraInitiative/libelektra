kdb-list(1) -- List plugins available to Elektra
================================================

## SYNOPSIS

`kdb list`

## DESCRIPTION

This command will list all the available Elektra plugins.
The output will be sorted by their status.
The best plugins will be the last in the list.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-v`, `--verbose`:
  Also output the number calculated by their
  `infos/status` clause in the contract.
- `-0`, `--null`:
  Use binary 0 termination

## EXAMPLES

To get a sorted list all available plugins with their status:
`kdb list -v`

## SEE ALSO

- `infos/status` is part of the [elektra-contracts.md (7)](elektra-contracts.md)
