# kdb-list-tools(1) - List all external tools available to Elektra

## SYNOPSIS

`kdb list-tools`

## DESCRIPTION

This command lists all the external tools that are available to Elektra.
In the first line it prints where external tools are located.

The tool itself is extern.

## ENVIRONMENT

- `KDB_EXEC_PATH`:
  Path to additional external tools.
  Can contain multiple paths separated with :.

## OPTIONS

Currently no option provided.

## SEE ALSO

- `kdb list-commands` to list internal kdb commands
