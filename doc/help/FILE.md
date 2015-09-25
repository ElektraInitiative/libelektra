kdb-file(1) -- Displays which file a key is stored in
=====================================================

## DESCRIPTION

This command prints which file a given key is stored in.  
While many keys are stored in a default key database file, many others are stored in any number of configuration files located all over the system.  
This tool is made to allow users to find out the file that a key is actually stored in.  

## USAGE

`kdb file <path>`  

Where `path` is the path of a key.  

## EXAMPLES

To find which file a key is stored in:  
	`kdb user/example/key`  

