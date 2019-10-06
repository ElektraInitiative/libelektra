# kdb-vset(1) - Set the value of a key with a validation regular expression

## SYNOPSIS

`kdb vset <key name> <value> <regex> [<message>]`

Where `key name` is the name of the key the user wishes to set, `value` is the value the user wishes to set, and `regex` is the regular expression that should be used for validation.
The optional parameter `message` is a user-defined message that will be displayed when a user tries to set the key to a value that doesn't match the regular expression.
The expression will be matched against the whole value (`check/validation/match=LINE`).

## DESCRIPTION

This command allows the user to set the value of a key and create a validation regular expression which future sets are checked against.
This command supports regular expressions as defined in extended regular expressions.
If a user tries to set a value that does not match the regular expression, a user-defined message is returned.

Note: In order for this command to work, the `validation` plugin must be mounted where the key resides.

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

## EXAMPLES

To mount the necessary validation plugin use:<br>
`kdb mount validation.ini /validation ini validation`

To set the `user/validation/key` key to the value `a` and validate that any future sets must match the regular expression `a+`:<br>
`kdb vset user/validation/key a a+ "The value of this key must only consist of one more of the letter a"`

## SEE ALSO

- Use `kdb plugin-info validation` to get information about the validation plugin.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
