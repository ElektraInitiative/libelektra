# Understanding Namespaces #

## Structure of the Key Database ##

The _key database_ of Elektra is _hierarchically structured_. This means that keys are organized similar to directories in a file system.

Lets add some keys to the database. To add a key we can use `kdb`, the _key database access tool_.

```sh
kdb set <key> <value>
```

Now add the the key **/a** with the Value **Value 1** and the key **/b/c** with the Value **Value 2**:

```sh
kdb set /a 'Value 1'
#> Using name user/a
#> Create a new key user/a with string Value 1
kdb set /b/c 'Value 2'
#> Using name user/b/c
#> Create a new key user/b/c with string Value 2
```

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
- **system** for system keys in `/etc` or `/`

All namespaces save their keys in a _separate hierarchical structure_ from the other namespaces.

But when we set the keys **/a** and **/b/c** before we didn't provide a namespace.
So I hear you asking, if every key has to belong to a namespace, where are the keys?
They are in the _user_ namespace, as you can verify with:

```sh
kdb ls user | grep -E '(/a|/b/c)'
#> user/a
#> user/b/c
```

When we don't provide a namespace Elektra assumes a default namespace, which should be **user** for non-root users.
So if you are a normal user the command `kdb set /b/c 'Value 2'` was synonymous to `kdb set user/b/c 'Value 2'`.

At this point the key database should have this structure:
![Elektras namespaces](/doc/images/tutorial_namespaces_namespaces.svg)

#### Cascading Keys ####

Another question you may ask yourself now is, what happens if we lookup a key without providing a namespace. So let us retrieve the key **/b/c** with the -v flag in order to make _kdb_ more talkative.

```sh
kdb get -v /b/c
# STDOUT-REGEX: got \d+ keys
#>  searching spec/b/c, found: <nothing>, options: KDB_O_CALLBACK
#>  searching proc/b/c, found: <nothing>, options:
#>  searching dir/b/c, found: <nothing>, options:
#>  searching user/b/c, found: user/b/c, options:
#> The resulting keyname is user/b/c
#> Value 2
```

Here you see how Elektra searches all namespaces for matching keys in this order:
**spec**, **proc**, **dir**, **user** and finally **system**

If a key is found in a namespace, it masks the key in all subsequent namespaces, which is the reason why the system namespace isn't searched. Finally the virtual key **/b/c** gets resolved to the real key **user/b/c**.
Because of the way a key without a namespace is retrieved, we call keys, that start with '**/**' **cascading keys**.
You can find out more about cascading lookups [here](cascading.md).



Having namespaces enables both admins and users to set specific parts of the application's configuration, as you will see in the following example.

## How it Works on the Command Line (kdb) ##

We will provide an example of how you can configure [elektrified](../help/elektra-glossary.md) applications.

Our exemplary application will be the key database access tool `kdb` as this should already be installed on your system.

`kdb` can be configured by the following configuration data:

- **/sw/elektra/kdb/#X/PROFILE/verbose** - sets the verbosity of kdb
- **/sw/elektra/kdb/#X/PROFILE/quiet** - if kdb should suppress non-error messages

**X** and **PROFILE** are placeholders for the _major version number_ and the _profile_ to which this configuration applies. If we want to set configuration for the default profile we can set PROFILE to %. The name of the key follows the convention described [here](../help/elektra-key-names.md).

Say we want to set `kdb` to be more verbose in the current directory by default. In this case we have to set _verbose_ to 1 in your dir namespace.
```sh
kdb set dir/sw/elektra/kdb/#0/%/verbose 1
#> Create a new key dir/sw/elektra/kdb/#0/%/verbose with string 1
```

If we now search for some key, `kdb` will behave just as if we have called it with the `-v` option.
```sh
kdb get /some/key
# STDOUT-REGEX: got \d+ keys
#> searching spec/some/key, found: <nothing>, options: KDB_O_CALLBACK
#>     searching proc/some/key, found: <nothing>, options:
#>     searching dir/some/key, found: <nothing>, options:
#>     searching user/some/key, found: <nothing>, options:
#>     searching system/some/key, found: <nothing>, options:
#>     searching default of spec/some/key, found: <nothing>, options: KDB_O_NOCASCADING
#> Did not find key
```

Verbosity is not always useful because it distracts from the essential.
So we may decide that we want `kdb` to be only verbose if we are debugging it.
So lets move the default configuration to another profile:
```sh
kdb mv -r dir/sw/elektra/kdb/#0/% dir/sw/elektra/kdb/#0/debug
#> using common basename: dir/sw/elektra/kdb/#0
#> key: dir/sw/elektra/kdb/#0/%/verbose will be renamed to: dir/sw/elektra/kdb/#0/debug/verbose
#> Will write out:
#> dir/sw/elektra/kdb/#0/debug/verbose
```

If we now call `kdb get /some/key` it will behave non-verbose, but if we call it with the _debug_ profile `kdb get -p debug /some/key` the configuration under **/sw/elektra/kdb/#0/debug** applies.

We configured kdb only for the current directory. If we like this configuration we could move it to the system namespace, so that every user can enjoy a preconfigured _debug_ profile.
```sh
sudo kdb mv -r dir/sw/elektra/kdb system/sw/elektra/kdb
#> using common basename: /sw/elektra/kdb
#> key: dir/sw/elektra/kdb/#0/%/verbose will be renamed to: system/sw/elektra/kdb/#0/%/verbose
#> Will write out:
#> system/sw/elektra/kdb/#0/%/verbose
```

Now every user can use the _debug_ profile with kdb.

_Cascading keys_ are keys that start with **/** and are a way of making key lookups much easier.
Let's say you want to see the configuration from the example above.
You do not need to search every namespace by yourself.
Just make a lookup for **/sw/elektra/kdb/#0/debug/verbose**, like this:

```sh
kdb get /sw/elektra/kdb/#0/debug/verbose
#> 1
```

When using cascading key the best key will be searched at runtime.
If you are only interested in the system key, you would use:

```sh
kdb get system/sw/elektra/kdb/#0/debug/verbose
#> 1
```

Because of _cascading keys_ a user can override the behavior of the _debug_ profile by setting the corresponding keys in his _user_ namespace
(as we discussed [before](#cascading-keys)).
If a user sets _verbose_ in his user namespace to 0 he overrides the default behavior from the _system_ namespace.

```sh
kdb set user/sw/elektra/kdb/#0/debug/verbose 0
#> Create a new key user/sw/elektra/kdb/#0/debug/verbose with string 0
kdb get /sw/elektra/kdb/#0/debug/verbose
#> 0
```

Now `kdb get -p debug /some/key` is not verbose anymore for this user.


## How it Works in C ##

The idea is to call **kdbGet()** to retrieve the root key for the application.
Looking for a specific part of the configuration is done by **ksLookup()**.

The documentation provides the following example to illustrate the intended usage.
If you want to use a _cascading key_ (starting with `/`),
you use the **[ksLookup()](https://doc.libelektra.org/api/current/html/group__keyset.html#gaa34fc43a081e6b01e4120daa6c112004)** or **[ksLookupByName()](https://doc.libelektra.org/api/current/html/group__keyset.html#gad2e30fb6d4739d917c5abb2ac2f9c1a1)** function:

```c
if (kdbGet (handle, myConfig,  p=keyNew("/sw/tests/myapp/#0/current", KEY_END)) == -1)
	errorHandler ("Could not get Keys", parentKey);
if ((myKey = ksLookupByName (myConfig, "/sw/tests/myapp/#0/current/key", 0)) == NULL)
	errorHandler ("Could not Lookup Key");
```
