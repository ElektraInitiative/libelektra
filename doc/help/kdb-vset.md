kdb-vset(1) - Set the value of a key with a validation regular expression
=========================================================================

## SYNOPSIS

`kdb vset <path> <value> <regex> [<message>]`

Where `path` is the path to the key the user wishes to set, `value` is the value the user wishes to set, and `regex` is the regular expression that should be used for validation.
The optional parameter `message` is a user-defined message that will be displayed when a user tries to set the key to a value that doesn't match the regular expression.


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
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.


## EXAMPLES

To set the `user/example/key` key to the value `a` and validate that any future sets must match the regular expression `a*`:
`kdb vset user/example/key a a+ "The value of this key must only consist of one more of the letter a"`

For more information about regular expressions with the `validation` plugin see:
`kdb info validation`

## SEE ALSO

- Use `kdb info validation` to get information about the validation plugin.
