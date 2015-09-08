kdb-cp(1) -- Copy keys within the key database
================================

## USAGE
`kdb cp <source> <dest>`  

Where `source` is the path of the key(s) you want to copy and `dest` is the path where you would like to copy the key(s) to.

## EXAMPLES

To copy a KeySet:  
	`kdb cp -r user\example1 user\example2`  

To copy a single key:  
	`kdb cp user\example\key1 user\example\key2`  
	


