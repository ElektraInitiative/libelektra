kdb-check(1) -- Perform internal checks
=======================================

## SYNOPSIS

`kdb check [<plugin>]`  

## DESCRIPTION

This command is used to perform checks on the key database or an Elektra plugin.  

Where the option argument, `plugin` is the plugin that a user wants to check.  
Use `-c` to pass options to that plugin.  
If no `plugin` argument is provided a check will be performed on the key database itself.  
Special values are returned upon exit to represent the outcome of a check.  

## OPTIONS

- `-H` `--help`:                Print help text.
- `-f` `--force`:
   The user can also use this tool to perform write tests. Please note that this can result in configuration files being changed!
- `-v` `--verbose`:             Explain what is happening.
- `-V` `--version`:             Print version info.
- `-c` `--plugins-config`:      Add a plugin configuration.


## RETURN VALUES

Their are two different types of checks, a check on a plugin (by specifying the name of a plugin as an argument) or a check on the key database itself.  

The outcome of a check on the key database is returned as an exit status.  
This integer represents an 8-bit pattern.  
Each bit represents a specific outcome as described below:  

 * 0:
   No errors (no output)  

 * Bit 1: 
   Warning on opening the key database.  

 * Bit 2:
   Error on opening th ekey database.  

 * Bit 3:
   Warning on getting the value of a key.  

 * Bit 4:
   Error on getting the value of a key.  

 * Bit 5:
   Warning on setting the value of a key. (only checked when `-f` is used)  

 * Bit 6:
   Error on setting the value of a key (only checked when `-f` is used)  

 * Bit 7:
   Warning on closing the key database.  

 * Bit 8:
   Error on closing the key database.  

So if the following number was returned `9` the user could figure out more detail by considering the bits: `00001001`  
The user would know that their was a warning on open and an error on get.  

If a plugin name is given, checks will only be done on the given plugin.  
The returned values for a check on a plugin are returned as much simpler numbers.  

Return values on plugin checking:  

 * 0:
   Everything ok. (no output)  

 * 1:
   No such plugin found or plugin could not be opened.  

 * 2:
   Plugin did not pass checks.  

 * 3:
   Plugin has warnings.  

Please report any output caused by official plugins to [http://git.libelektra.org/issues](http://git.libelektra.org/issues).  

Since the error code is a return value, it is not automatically displayed to the shell.  
If the user wants to have the value printed, they must do so manually (by running a command such as `echo $?`.   


## EXAMPLES

To check the Key Database:  
	`kdb check`  

To check the Key Database and then print the result:  
	`kdb check`  
followed by:  
	`echo $?`  

To check the Key Database including write checks:  
	`kdb check -f`  
Note that this type of check may change configuration files.  

To check the `line` plugin:  
	`kdb check line`  

