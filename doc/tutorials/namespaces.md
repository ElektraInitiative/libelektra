# Namespaces #

## Definition ##

In Elektra multiple key may refer to the same part of the configuration.
A _namespace_ is the part of the key that refers to the key's origin (like a unique path).

Possible sources are: 

- ```spec/something``` for specification of other keys
- ```proc/something``` for in-memory keys (e.g. command-line)
- ```dir/something``` for ```dir``` keys in current working directory
- ```system/something``` for system keys in ```/etc``` or ```/```
- ```user/something``` for user keys in home directory
- ```/something``` for cascading keys (actually refers to one of the above)

Having namespaces enables both admins and users to set specific parts of the application's configuration.

## How it works ##

The idea is to call ```kdbGet()``` to retrieve the root key for the application.
Looking for a specific part of the configuration is done by ```ksLookup()```.

The documentation provides the following example to illustrate the intended usage:

	if (kdbGet(handle, "user/myapp", myConfig, 0 ) == -1)
		errorHandler ("Could not get Keys");
	if (kdbGet(handle, "system/myapp", myConfig, 0 ) == -1)
		errorHandler ("Could not get Keys");
	if ((myKey = ksLookup(myConfig, key, 0)) == NULL)
		errorHandler ("Could not Lookup Key");

