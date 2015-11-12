kdb-rm(1) -- Remove key(s) from the key database
================================================

## DESCRIPTION

This command removes key(s) from the Key database.  

## USAGE

`kdb rm <path>`  

Where `path` is the path of the key(s) you want to remove.  
Note that when using the `-r` flag, `path` as well as all of the keys below it will be removed.  

## EXAMPLES

To remove a multiple keys:  
	`kdb rm -r user/example`  

To remove a single key:  
	`kdb rm user/example/key1`  
