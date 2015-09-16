# Namespaces #

## Definition ##

In Elektra multiple keys may refer to the same part of the configuration.
A _namespace_ is the part of the key that refers to the key's origin (like a unique path).

Possible sources are: 

- **spec/something** for specification of other keys
- **proc/something** for in-memory keys (e.g. command-line)
- **dir/something** for dir keys in current working directory
- **system/something** for system keys in /etc or /
- **user/something** for user keys in home directory
- **/something** for cascading keys (actually refers to one of the above)

Having namespaces enables both admins and users to set specific parts of the application's configuration.

## How it works on the command line (kdb) ##

Let's say your app requires the following configuration data:

- **sw/org/myapp/policy** - a security policy to be applied
- **sw/org/myapp/default_dir** - a place where the application stores its data per default

We now want to enter this configuration by using the **kdb** tool.

The security policy will most probably be set by your system administrator.
So she enters

	sudo kdb set "system/sw/org/myapp/policy" "super-high-secure"

The key **system/app/policy** will be stored in the system namespace (probably at /etc/kdb on a Linux/UNIX system).

Then the user sets his app directory by issuing:

	kdb set "user/sw/org/myapp/default_dir" "/home/user/.myapp"

This key will be stored in the user namespace (at the home directory) and thus may vary from user to user.
Elektra loads the value for the current user and passes it to the application.

You can also retrieve the values in the command line by using the **kdb** tool:

	kdb get system/sw/org/maypp

_Cascading keys_ are keys that start with **/** and are a way of making key lookups much easier.
Let's say you want to load the configuration from the example above.
You do not need to search every namespace by yourself.
Just make a lookup for **/sw/org/myapp**, like this:

	kdb get /sw/org/myapp/policy
	kdb get /sw/org/myapp/default_dir

When using cascading key the best key will be searched at runtime.
If you are only interested in the system key, you would use:

	kdb get system/sw/org/myapp/policy

## How it works in C ##

The idea is to call **kdbGet()** to retrieve the root key for the application.
Looking for a specific part of the configuration is done by **ksLookup()**.

The documentation provides the following example to illustrate the intended usage.
If you want to use a _cascading key_ (starting with /),
you use the **ksLookup()** or **ksLookupByName()** function
(also see [doxygen](http://doc.libelektra.org/api/current/html/group__keyset.html#gaa34fc43a081e6b01e4120daa6c112004) ):

	if (kdbGet(handle, myConfig,  p=keyNew("/sw/org/myapp", KEY_END)) == -1)
		errorHandler ("Could not get Keys", parentKey);
	if ((myKey = ksLookupByName (myConfig, "/sw/org/myapp/mykey", 0)) == NULL)
		errorHandler ("Could not Lookup Key");

