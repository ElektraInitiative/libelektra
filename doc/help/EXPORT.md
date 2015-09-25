kdb-export(1) -- Export keys from the key database
==================================================
## DESCRIPTION

This command allows a user to export keys from the key database.  
Keys are exported to `stdout` in whichever format is specified.  
The default format is `dump` but can be changed by editing the value of the `sw/kdb/current/format` key.  
This command can also be used to view full key(s) including their values.  

## USAGE

`kdb export <source> [<format>]`  

Where `source` is the path of the key(s) you want to export.  
Additionally, the user can specify a format to use by passing it as the option argument `format`.  

## EXAMPLES

To view your full key database in Elektra's native `dump` format:
	`kdb export /`  

To backup your full key database in Elektra's native `dump` format to a file called `full-backup.ecf`:
	`kdb export / > full-backup.ecf`  

To view a keyset stored in `user/keyset` in the XML format:
	`kdb export user/keyset xmltool`  

To backup a keysey stored in `user/keyset` in the `ini` format to a file called `keyset.ini`:
	`kdb export user/keyset ini > keyset.ini`  

