kdb-cp(1) -- Copy keys within the key database
================================
## DESCRIPTION
This command copies key(s) in the Key database.  
You can copy keys to another directory within the database or even below another key.  
Note that you can't copy a key below itself.  


## USAGE
`kdb cp <source> <dest>`  

Where `source` is the path of the key(s) you want to copy and `dest` is the path where you would like to copy the key(s) to.

## EXAMPLES

To copy a KeySet:  
	`kdb cp -r user\example1 user\example2`  

To copy a single key:  
	`kdb cp user\example\key1 user\example\key2`  

To copy keys below an aexisting key:  
	`kdb cp -r user\example user\example\key1`  
Note that in this example, all keys in the example directory will be copied below `key1` EXCEPT `key1`.  
	


