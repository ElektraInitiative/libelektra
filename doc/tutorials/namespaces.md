# Understanding Namespaces #

## Structure of the key database ##

The _key database_ of Elektra is _hierarchically structured_. This means that keys are organized similar to directories in a file system.

Lets add some keys to the database. To add a key we can use this command:

	kdb set <key> <value>

Now add the the key **/a** with the Value **Value 1** and the key **/b/c** with the Value **Value 2**:

	kdb set /a 'Value 1'
	kdb set /b/c 'Value 2'

![Hierarchical structure of key database](/doc/images/tutorial_namespaces_hierarchy.svg)

Here you see the internal structure of the database after adding the keys **/a** and **/b/c**.
For instance the key **/b/c** has the path **/** -> **b** -> **c**.

Note how the name of the key determines the path to its value.

You can use the file system analogy as a mnemonic to remember these commands (like the file system commands in your favorite operating system):
- `kdb ls <path>`
	lists keys below _path_
- `kdb rm <key>`
	removes a _key_
- `kdb cp <source> <dest>`
	copies a key to another path
- `kdb get <key>`
	gets the value of _key_

For example `kdb get /b/c` should return `Value 2` now, if you set the values before.

## Namespaces ##

Now we abandon the file system analogy and introduce the concept of _namespaces_.

Every key in Elektra belongs to one of these namespaces:
- **spec** for specification of other keys
- **proc** for in-memory keys (e.g. command-line)
- **dir** for dir keys in current working directory
- **user** for user keys in home directory
- **system** for system keys in /etc or /

All namespaces save their keys in a _separate hierarchical structure_ from the other namespaces.

But when we set the keys **/a** and **/b/c** before we didn't provide a namespace.
So I hear you asking, if every key has to belong to a namespace, where are the keys?
They are in the _user_ namespace, as you can verify with:

	kdb ls user
	# user/a
	# user/b/c

When we don't provide a namespace Elektra assumes a default namespace, which should be **user** for non-root users.
So if you are a normal user the command `kdb set /b/c 'Value 2'` was synonymous to `kdb set user/b/c 'Value 2'`.

At this point the key database should have this structure:
![Elektras namespaces](/doc/images/tutorial_namespaces_namespaces.svg)

#### Cascading keys ####
Another question you may ask yourself now is, what happens if we lookup a key without providing a namespace. So let us retrieve the key **/b/c** with the -v flag in order to make _kdb_ more talkative.

	kdb get -v /b/c
	# got 3 keys
	#  searching spec/b/c, found: <nothing>, options: KDB_O_CALLBACK
	#  searching proc/b/c, found: <nothing>, options:
	#  searching dir/b/c, found: <nothing>, options:
	#  searching user/b/c, found: user/b/c, options:
	# The resulting keyname is user/b/c
	# Value 2

Here you see how Elektra searches all namespaces for matching keys in this order:
**spec**, **proc**, **dir**, **user** and finally **system**

If a key is found in a namespace, it masks the key in all subsequent namespaces, which is the reason why the system namespace isn't searched. Finally the virtual key **/b/c** gets resolved to the real key **user/b/c**.
Because of the way a key without a namespace is retrieved, we call keys, that start with '**/**' **cascading keys**.
You can find out more about cascading lookups [here](cascading.md).



Having namespaces enables both admins and users to set specific parts of the application's configuration, as you will see in the following example.

## How it works on the command line (kdb) ##

Let's say your app requires the following configuration data:

- **/sw/org/myapp/policy** - a security policy to be applied
- **/sw/org/myapp/default_dir** - a place where the application stores its data per default

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
