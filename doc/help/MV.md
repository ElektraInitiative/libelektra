kdb-mv(1) -- Move keys within the key database
==============================================

## DESCRIPTION

This command moves key(s) in the Key database.  
You can move keys to another directory within the database or even below another key.  

## USAGE

`kdb mv <source> <dest>`  

Where `source` is the path of the key(s) you want to copy and `dest` is the path where you would like to move the key(s) to.  

## EXAMPLES

To move a KeySet:  
	`kdb mv -r user/example1 user/example2`  

To move a single key:  
	`kdb mv user/example/key1 user/example/key2`  


