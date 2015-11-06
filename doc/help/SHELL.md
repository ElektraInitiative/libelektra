kdb-shell(1) -- Start a kdb shell instance
==========================================

## DESCRIPTION

This command is used to start an instance of the kdb shell.  
The kdb shell allows for a user to interacively view, edit, or otherwise work with the key database.  

## USAGE

`kdb shell`  

## SHELL COMMANDS ##

The kdb shell offers a number of commands to interact with the key database.  

* kdbGet <name>:  
  Get the value of a key.  

* kdbSet <name>:
  Set the value of a key.  

* keySetName <name>:
  Set the name of the current key.  

* keySetMeta <name> <string>:  
  Set a meta key associated with the current key.  

* keySetString <string>:  
  Set a string value for the current key.  

* ksAppendKey:  
  Append the current key to the current keyset.  

* ksCut <name>:  
  Cut the current keyset.  

* ksOutput:  
  Prints the keys in the current keyset.  

To learn more about these commands and how they work, refer to the [Elektra API Documentation](http://doc.libelektra.org/api/current/html).

