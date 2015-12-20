kdb-info(1) -- Print information about an Elektra plugin
========================================================

## SYNOPSIS

`kdb info <plugin> [<clause name>]`  

Where `plugin` is the plugin in which the user would like to know information about.  
The optional `clause name` argument can be used to just print information from a certain clause.  

## DESCRIPTION

This command will print out all the information about an Elektra plugin except it's configuration.  
This command will also print out any functions that are exported by the plugin.  
Information about a plugin will be read from `system/elektra/modules/`. If the information could not be located there, such as when a plugin is not mounted, the module will be loaded dynamically and then the information will be requested directly from the plugin.  
If a user wishes to load the information directly from the plugin, they can force that by using the `-l` option.  

## RETURN VALUES

This command returns the following exit statuses:  

* 0:  
  The command was successful.  

* 1:  
  A `clause name` was specified but not found.  

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-l`, `--load`:
  Load plugin even if system/elektra is available.
- `-c`, `--plugins-config`:
  Add a plugin configuration.

## EXAMPLES

To print all the information about the `dump` plugin:  
	`kdb info dump`  

To print out the license of the `resolver` plugin directly by forcing it to load:  
	`kdb info -l resolver licence`  

To print out the author of the `line` plugin:  
	`kdb info line author`  

