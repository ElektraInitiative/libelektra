kdb-export(1) -- Export keys from the key database
==================================================

## SYNOPSIS

`kdb export <source> [<format>]`  

## DESCRIPTION

This command allows a user to export keys from the key database.  
Keys are exported to `stdout` in whichever format is specified.  
The default format is `dump` but can be changed by editing the value of the `sw/kdb/current/format` key.  
This command can also be used to view full key(s) including their values.  

## USAGE

Where `source` is the path of the key(s) you want to export.  
Additionally, the user can specify a format to use by passing it as the option argument `format`.  
The `format` attribute relies on Elektra's plugin system to export the keys in the desired format. The user can view all plugins available for use by running the kdb-list(1) command. To learn about any plugin, the user can simply use the kdb-info(1) command.  


## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-E`, `--without-elektra`:
  Omit the `system/elektra` directory.
- `-c`, `--plugins-config`:
  Add a configuration to the format plugin.

## EXAMPLES

To view your full key database in Elektra's native `dump` format:
	`kdb export /`  

To backup your full key database in Elektra's native `dump` format to a file called `full-backup.ecf`:
	`kdb export / > full-backup.ecf`  

To view a keyset stored in `user/keyset` in the XML format:
	`kdb export user/keyset xmltool`  

To backup a keyset stored in `user/keyset` in the `ini` format to a file called `keyset.ini`:
	`kdb export user/keyset ini > keyset.ini`  


## SEE ALSO

- For an introductions into plugins, read [elektra-plugins(7)](elektra-plugins.md).
- Only storage plugins can be used for formatting.
