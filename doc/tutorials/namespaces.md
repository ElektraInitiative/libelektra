# Understanding Namespaces

## Structure of the Key Database

The _key database_ of Elektra is _hierarchically structured_. This means that keys are organized similar to directories in a file system.

Let us add some keys to the database. To add a key we can use `kdb`, the _key database access tool_:

```sh
kdb set <key> <value>
```

Now add the the key **/a** with the Value **Value 1** and the key **/b/c** with the Value **Value 2**:

```sh
kdb set /a 'Value 1'
#> Using name user/a
#> Create a new key user/a with string "Value 1"
kdb set /b/c 'Value 2'
#> Using name user/b/c
#> Create a new key user/b/c with string "Value 2"
```

<img src="https://cdn.rawgit.com/ElektraInitiative/libelektra/master/doc/images/tutorial_namespaces_hierarchy.svg" alt="Hierarchical structure of key database" />

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

## Namespaces

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
<img src="https://cdn.rawgit.com/ElektraInitiative/libelektra/master/doc/images/tutorial_namespaces_namespaces.svg" alt="Elektra’s namespaces" />

### Cascading Keys

Another question you may ask yourself now is, what happens if we lookup a key without providing a namespace. So let us retrieve the key **/b/c** with the -v flag in order to make _kdb_ more talkative.

```sh
kdb get -v /b/c
# STDOUT-REGEX: got \d+ keys
#>  searching spec/b/c, found: <nothing>, options: KDB_O_CALLBACK
#>  searching proc/b/c, found: <nothing>, options:
#>  searching dir/b/c, found: <nothing>, options:
#>  searching user/b/c, found: user/b/c, options:
#> The resulting key name is user/b/c
#> Value 2
```

Here you see how Elektra searches all namespaces for matching keys in this order:
**spec**, **proc**, **dir**, **user** and finally **system**

If a key is found in a namespace, it masks the key in all subsequent namespaces, which is the reason why the system namespace isn't searched. Finally the virtual key **/b/c** gets resolved to the real key **user/b/c**.
Because of the way a key without a namespace is retrieved, we call keys, that start with '**/**' **cascading keys**.
You can find out more about cascading lookups in the [cascading tutorial](cascading.md).

Having namespaces enables both admins and users to set specific parts of the application's configuration, as you will see in the following example.

## How it Works on the Command Line (kdb)

We will provide an example of how you can configure [elektrified](/doc/help/elektra-glossary.md) applications.

Our exemplary application will be the key database access tool `kdb` as this should already be installed on your system.

`kdb` can be configured by the following configuration data:

- _/sw/elektra/kdb/#**X**/**PROFILE**/verbose_ - sets the verbosity of kdb
- _/sw/elektra/kdb/#**X**/**PROFILE**/quiet_ - if kdb should suppress non-error messages
- _/sw/elektra/kdb/#**X**/**PROFILE**/namespace_ - specifies the default namespace used, when setting a cascading name

**X** is a placeholder for the _major version number_ and **PROFILE** stands for the name of a _profile_ to which this configuration applies. If we want to set configuration for the default profile we can set **PROFILE** to %. The name of the key follows the convention described [here](/doc/help/elektra-key-names.md).

Say we want to set `kdb` to be more verbose when it is used in the current directory. In this case we have to set _verbose_ to 1 in the _dir_ namespace of the current directory.

```sh
kdb set "dir/sw/elektra/kdb/#0/%/verbose" 1
#> Create a new key dir/sw/elektra/kdb/#0/%/verbose with string "1"
```

> The configuration for a directory is actually stored in this directory.
> By default the configuration is contained in a folder named `.dir`, as you can verify with `kdb file dir` (_kdb file_ tells you the file where a key is stored in).
>
> For the purpose of demonstration we chose to only manipulate the verbosity of kdb.
> Note that setting `dir/sw/elektra/kdb/#0/%/namespace` to `dir` can be handy if you want to work with configuration of an application in a certain directory.

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
So let us move the default configuration to another profile:

```sh
kdb mv -r "dir/sw/elektra/kdb/#0/%" "dir/sw/elektra/kdb/#0/debug"
#> using common basename: dir/sw/elektra/kdb/#0
#> key: dir/sw/elektra/kdb/#0/%/verbose will be renamed to: dir/sw/elektra/kdb/#0/debug/verbose
#> Will write out:
#> dir/sw/elektra/kdb/#0/debug/verbose
```

If we now call `kdb get /some/key` it will behave non-verbose, but if we call it with the _debug_ profile `kdb get -p debug /some/key` the configuration under **/sw/elektra/kdb/#0/debug** applies.

We configured kdb only for the current directory. If we like this configuration we could move it to the system namespace, so that every user can enjoy a preconfigured _debug_ profile.

```sh
sudo kdb mv -r "dir/sw/elektra/kdb" "system/sw/elektra/kdb"
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
kdb get "/sw/elektra/kdb/#0/debug/verbose"
#> 1
```

When using cascading key the best key will be searched at run-time.
If you are only interested in the system key, you would use:

```sh
kdb get "system/sw/elektra/kdb/#0/debug/verbose"
#> 1
```

Because of _cascading keys_ a user can override the behavior of the _debug_ profile by setting the corresponding keys in his _user_ namespace
(as we discussed [before](#cascading-keys)).
If a user sets _verbose_ in his user namespace to 0 she overrides the default behavior from the _system_ namespace.

```sh
kdb set "user/sw/elektra/kdb/#0/debug/verbose" 0
#> Create a new key user/sw/elektra/kdb/#0/debug/verbose with string "0"
kdb get "/sw/elektra/kdb/#0/debug/verbose"
#> 0
```

Now `kdb get -p debug /some/key` is not verbose anymore for this user.
