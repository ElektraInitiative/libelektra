# kdb-export(1) -- Export keys from the key database

## SYNOPSIS

`kdb export <source> [<format>]`<br>

## DESCRIPTION

This command allows a user to export keys from the key database.<br>
Keys are exported to `stdout` in whichever format is specified.<br>
This command can also be used to view full key(s) including their values.<br>

## USAGE

Where `source` is the path of the key(s) you want to export.
Additionally, the user can specify a format to use by passing it as the option argument `format`.<br>
The `format` attribute relies on Elektra’s plugin system to export the keys in the desired format.The user can view all plugins available for use by running the kdb-plugin-list(1) command. To learn about any plugin, the user can simply use the kdb-plugin-info(1) command.<br>
The `storage` plugin can be configured at compile-time or changed by the link `libelektra-plugin-storage.so`.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-E`, `--without-elektra`:
  Omit the `system:/elektra` directory.
- `-c`, `--plugins-config <plugins-config>`:
  Add a configuration to the format plugin.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## KDB

- `/sw/elektra/kdb/#0/current/format`
  Change default format (if none is given at commandline) and built-in default `storage` is not your preferred format.

## EXAMPLES

To view your full key database in Elektra’s `storage` format:<br>
`kdb export /`<br>

To backup your full key database in Elektra’s `storage` format to a file called `full-backup.ecf`:<br>
`kdb export / > full-backup.ecf`<br>

To backup a keyset stored in `user:/keyset` in the `ini` format to a file called `keyset.ini`:<br>
`kdb export user:/keyset ini > keyset.ini`<br>

Change default format to `simpleini`:<br>
`kdb set /sw/elektra/kdb/#0/current/format simpleini`

Create two key values and export them as `xml`:

```sh
kdb set user:/tests/kdb-export/one one
kdb set user:/tests/kdb-export/two two

kdb export user:/tests/kdb-export/ xml
#> <?xml version="1.0" encoding="UTF-8" standalone="no" ?>
#> <kdb-export>
#>
#>   <one>one</one>
#>
#>   <two>two</two>
#>
#> </kdb-export>


kdb rm -r user:/tests
# cleanup
```

Create two key values and export them with the `xerces` plugin:

```sh
kdb set user:/tests/kdb-export/one one
kdb set user:/tests/kdb-export/two two

kdb export user:/tests/kdb-export/ xerces
#> <?xml version="1.0" encoding="UTF-8" standalone="no" ?>
#> <kdb-export>
#>
#>   <one>one</one>
#>
#>   <two>two</two>
#>
#> </kdb-export>

kdb rm -r user:/tests
# cleanup
```

## Note

- Only storage plugins can be used for formatting.
