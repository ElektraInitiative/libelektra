kdb-rm(1) -- Remove key(s) from the key database
================================================

## DESCRIPTION

This command removes key(s) from the Key database.  

## USAGE

`kdb rm <path>`  

Where `path` is the path of the key(s) you want to remove.  

## EXAMPLES

To remove a KeySet:  
	`kdb rm -r user/example`  

To remove a single key:  
	`kdb rm user/example/key1`  
