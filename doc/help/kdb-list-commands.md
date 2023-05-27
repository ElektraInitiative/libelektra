# kdb-list-commands(1) -- List commands available to Elektra

## SYNOPSIS

`kdb list-commands`

## DESCRIPTION

This command will list all internal kdb commands.
The output is suitable to be processed from other
tools as long as `-v` is not used.

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Also output some explanation for every
  command. The command names are bold if
  color is turned on. In the first line
  it will add how many commands exist.
- `-0`, `--null`:
  Use binary 0 termination

## EXAMPLES

To get a list of all available commands with their help text:
`kdb list-commands -v`

## SEE ALSO

- `kdb list-tools` for external kdb commands
