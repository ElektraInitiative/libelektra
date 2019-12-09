# kdb-list-commands(1) -- List commands available to Elektra

## SYNOPSIS

`kdb list-commands [OPTION]...`

## DESCRIPTION

This command will list all internal kdb commands.
The output is suitable to be processed from other
tools unless `-v` or `--verbose` is specified.

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
  Prints the number of commands in the first line
  and adds a description to each command.
  Prints command names in bold unless `--color never` is specified or output is printed to non-terminals.
- `-0`, `--null`:
  Use binary 0 termination

## EXAMPLES

To print a list of all internal kdb commands with their description use:

`kdb list-commands -v`

## SEE ALSO

- `kdb list-tools` for external kdb commands
