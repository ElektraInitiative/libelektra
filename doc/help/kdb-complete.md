kdb-complete(1) -- Show suggestions how to complete a given path
================================

## SYNOPSIS

`kdb complete <path>`  

Where `path` is the path for which the user would like to receive completion suggestion.

## DESCRIPTION

Show suggestions how the current name could be completed.
Suggestions will include existing key names, path segments of existing key names and mountpoints.
Additionally, the output will indicate wheter the given path is a node or a leaf in the hierarchy of keys.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-m`, `--min-depth`=<min-depth>:
  Specify the minimum depth of completion suggestions (0 by default), exclusive.
- `-M`, `--max-depth`=<max-depth>:
  Specify the maximum depth of completion suggestions (unlimited by default, 1 to show only the next level), inclusive.
- `-v`, `--verbose`:
  Explain what is happening.
- `-0`, `--null`:
  Use binary 0 termination.
- `-d`, `--debug`:
  Give debug information.

## EXAMPLES

To show all possible completions for `user/te`:  
`kdb complete user/te`

To show all possible completions for the next level for `user/te`:  
`kdb complete --max-depth=1 user/te`

## SEE ALSO

- If you would only like to see existing keys under a given path, consider using the [kdb-ls(1)](kdb-ls.md) command.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.