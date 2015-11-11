kdb-set(1) -- Set the value of a key
====================================

## DESCRIPTION

This command allows the user to set the value of an individual key.  

## USAGE

`kdb set <path> [<value>]`  

Where `path` is the path to the key you wish to set the value of (or create) and `value` is the value you would like to set the key to.  
If the `value` argument is not passed, the key will be set to a value of `null`.  
Note: To set a key to an empty value, `""` should be passed for the `value` argument.  

## EXAMPLES

To set a Key to the value `Hello World!`:  
	`kdb set user/example/key "Hello World!"`  

To create a new key with a null value:  
	`kdb set user/example/key`  

To set a key to an empty value:
	`kdb set user/example/key ""`


