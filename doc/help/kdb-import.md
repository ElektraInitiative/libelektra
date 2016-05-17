kdb-import(1) -- Import an existing configuration into the key database
=======================================================================

## SYNOPSIS

`kdb import <destination> [<format>]`

Where `destination` is the destination where the user wants the keys to be imported into.
`format` is the format of the keys that are imported.

## DESCRIPTION

If the `format` argument is not passed, then the default format will be used as determined by the value of the `sw/kdb/current/format` key. By default, that key is set to the `storage` format.
The `format` attribute relies on Elektra's plugin system to properly import the configuration. The user can view all plugins available for use by running the kdb-list(1) command. To learn about any plugin, the user can simply use the kdb-info(1) command.

This command allows a user to import an existing configuration into the key database.
The configuration that the user wants to import is read from `stdin`.
The user should specify the format that the current configuration or keys are in, otherwise the default format will be used.
The default format is `storage` but can be changed by editing the value of the `sw/kdb/current/format` key.
The `storage` plugin can be configured at compile-time or changed by the link `libelektra-storage.so`.

## CONFLICTS

Conflicts can occur when importing a configuration to a part of the database where keys already exist.
Conflicts when importing can be resolved using a [strategy](#STRATEGIES) with the `-s` argument.

## STRATEGIES

Currently the following strategies exist for importing configurations:

- `cut`:
  Removes existing keys below `destination` and replaces them with the keys resulting from the import.
  This is the default strategy.

- `import`:
  Preserves existing keys below `destination` only if they do not exist in the keys being imported.
  If the key does exist in the imported keys, the current version will be overwritten.


## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `s`, `--strategy <name>`:
  Specify which strategy should be used to resolve conflicts.
- `-v`, `--verbose`:
  Explain what is happening.
- `-c`, `--plugins-config`:
  Add a configuration to the format plugin.
- `-C`, `--color`=[when]:
  Print never/auto(default)/always colored output.


## EXAMPLES

To import a configuration stored in the XML format in a file called `example.xml` below `user/keyset`:
`kdb import user/keyset xmltool < example.xml`

To import a configuration stored in the `ini` format in a file called `example.ini` below `user/keyset` replacing any previous keys stored there:
`cat example.ini | kdb import -s cut user/keyset ini`

To import a configuration stored in the `ini` format in a file called `example.ini` below `user/keyset` keeping any previous keys stored there that aren't present in the newly imported configuration:
`cat example.ini | kdb import -s import user/keyset ini`

To restore a backup (stored as `sw.ecf`) of a user's configuration below `system/sw`:
`cat sw.ecf | kdb import system/sw`

## SEE ALSO

- [elektra-merge-strategy(7)](elektra-merge-strategy.md)
