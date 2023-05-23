# kdb-import(1) -- Import an existing configuration into the key database

## SYNOPSIS

`kdb import <destination> [<format>]`

Where `destination` is the destination where the user wants the keys to be imported into.
`format` is the format of the keys that are imported.

## DESCRIPTION

If the `format` argument is not passed, then the default format will be used as determined by the value of the `sw/kdb/current/format` key. By default, that key is set to the `storage` format.
The `format` attribute relies on Elektra’s plugin system to properly import the configuration. The user can view all plugins available for use by running the kdb-plugin-list(1) command. To learn about any plugin, the user can simply use the kdb-plugin-info(1) command.

This command allows a user to import an existing configuration into the key database.
The configuration that the user wants to import is read from `stdin`.
The user should specify the format that the current configuration or keys are in, otherwise the default format will be used.
The default format is `storage` but can be changed by editing the value of the `/sw/elektra/kdb/#0/current/format` key.
The `storage` plugin can be configured at compile-time or changed by the link `libelektra-plugin-storage.so`.

## CONFLICTS

Conflicts can occur when importing a configuration to a part of the database where keys already exist.
Conflicts when importing can be resolved using a strategy with the `-s` argument.

The strategies implemented by the merge framework are documented in
[elektra-merge-strategy(7)](elektra-merge-strategy.md).

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-s`, `--strategy <name>`:
  Specify which strategy should be used to resolve conflicts.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.
- `-c`, `--plugins-config`:
  Add a configuration to the format plugin.

## KDB

- `/sw/elektra/kdb/#0/current/verbose`:
  Same as `-v`: Explain what is happening (output merged keys).

- `/sw/elektra/kdb/#0/current/format`
  Change default format (if none is given at commandline) and built-in default is not your preferred format.

## EXAMPLES

To import a configuration stored in the XML format in a file called `example.xml` below `user:/keyset`:<br>
`kdb import user:/keyset xmltool < example.xml`

To import a configuration stored in the `ini` format in a file called `example.ini` below `user:/keyset` replacing any previous keys stored there:<br>
`cat example.ini | kdb import -s cut user:/keyset ini`

To import a configuration stored in the `ini` format in a file called `example.ini` below `user:/keyset` keeping any previous keys stored there that aren't present in the newly imported configuration:<br>
`cat example.ini | kdb import -s import user:/keyset ini`

To restore a backup (stored as `sw.ecf`) of a user's configuration below `system:/sw`:<br>
`cat sw.ecf | kdb import system:/sw`

To import a sample `xml` content with the `xerces` plugin:

```sh
# import two keys from a xml string
kdb import user:/tests/kdb-import/ xerces <<< "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?><kdb-import><one>one</one><two>two</two></kdb-import>"

# get the values and verify they got imported correctly
kdb get user:/tests/kdb-import/one
#> one
kdb get user:/tests/kdb-import/two
#> two
kdb rm -r user:/tests/kdb-import
```

To import a sample `xml` content via specifying the file format directly:

```sh
# import two keys from a xml string
kdb import user:/tests/kdb-import/ xml <<< "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?><kdb-import><one>one</one><two>two</two></kdb-import>"

# get the values and verify they got imported correctly
kdb get user:/tests/kdb-import/one
#> one
kdb get user:/tests/kdb-import/two
#> two

kdb rm -r user:/tests/kdb-import
```

## SEE ALSO

- [elektra-merge-strategy(7)](elektra-merge-strategy.md)
