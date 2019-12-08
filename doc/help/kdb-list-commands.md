# kdb-list-commands(1) -- List commands available to Elektra

## SYNOPSIS

`kdb list-commands [OPTION]...`

## DESCRIPTION

This command will list all internal kdb commands.
The output is suitable to be processed from other
tools unless `--verbose` is specified.

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
  Print number of commands in the first line.
  Add a description to each command.
  Print command names bold unless `--color never` is specified.
- `-0`, `--null`:
  Use binary 0 termination

## EXAMPLES

To print a list of all internal kdb commands with their description:

`kdb list-commands -v`

## SEE ALSO

- `kdb list-tools` for external kdb commands
