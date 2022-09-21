# Understanding Namespaces

## Structure of the Key Database

The _key database_ of Elektra is _hierarchically structured_. This means that keys are organized similar to directories in a file system.

Let us add some keys to the database. To add a key we can use `kdb`, the _key database access tool_:

```shell
kdb set <key> <value>
```

Now add the key **user:/tests/a** with the Value **Value 1** and the key **user:/tests/b/c** with the Value **Value 2**

```sh
kdb set user:/tests/a 'Value 1'
#> Create a new key user:/tests/a with string "Value 1"
kdb set user:/tests/b/c 'Value 2'
#> Create a new key user:/tests/b/c with string "Value 2"
```

The database has an hierarchical structure.
For instance the key **user:/tests/b/c** has the path **user:/** -> **user:/tests** -> **user:/tests/b** -> **user:/tests/b/c**.

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

For example `kdb get user:/tests/b/c` should return `Value 2` now, if you set the values before.

## Namespaces

Now we abandon the file system analogy and introduce the concept of _namespaces_.

Above all of the keys have been prefixed with `user:/`.
The user namespace is one of several in Elektra.

Every key in Elektra belongs to one of these namespaces:

- **spec** for specification of other keys
- **proc** for in-memory keys (e.g. command-line)
- **dir** for dir keys in current working directory
- **user** for user keys in home directory
- **system** for system keys in `/etc` or `/`
- **default** for keys which are created with default value (if specification requires the key to exist, has default and key was missing)

All namespaces save their keys in a _separate hierarchical structure_ from the other namespaces.

But when we set the keys **/a** and **/b/c** before we didn't provide a namespace.
So I hear you asking, "if every key has to belong to a namespace, where are the keys?"
They are in the _user_ namespace, as you can verify with:

```sh
kdb ls user:/ | grep -E '(tests/a|tests/b/c)'
#> user:/tests/a
#> user:/tests/b/c
```

At this point the key database should have this structure:
<img src="https://cdn.rawgit.com/ElektraInitiative/libelektra/master/doc/images/tutorial_namespaces_namespaces.svg" alt="Elektra’s namespaces" />

### Cascading Keys

Another question you may ask yourself now is, what happens if we look up a key without providing a namespace. So let us retrieve the key **/b/c** with the -v flag in order to make _kdb_ more talkative.

```sh
kdb get -v /tests/b/c
# STDOUT-REGEX: got [[:digit:]]+ keys⏎searching spec:/tests/b/c, found: <nothing>, options: KDB_O_CALLBACK⏎    searching proc:/tests/b/c, found: <nothing>, options: KDB_O_CALLBACK⏎    searching dir:/tests/b/c, found: <nothing>, options: KDB_O_CALLBACK⏎    searching user:/tests/b/c, found: user:/tests/b/c, options: KDB_O_CALLBACK⏎The resulting keyname is user:/tests/b/c⏎The resulting value size is 8⏎Value 2
```

Here you see how Elektra searches all namespaces for matching keys in this order:
**spec**, **proc**, **dir**, **user** and finally **system**

If a key is found in a namespace, it masks the key in all subsequent namespaces, which is the reason why the system namespace isn't searched. Finally, the virtual key **/b/c** gets resolved to the real key **user:/b/c**.
Because of the way a key without a namespace is retrieved, we call keys, that start with '**/**', **cascading keys**.
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

```shell
kdb set "dir:/sw/elektra/kdb/#0/%/verbose" 1
#> Create a new key dir:/sw/elektra/kdb/#0/%/verbose with string "1"
```

> The configuration for a directory is actually stored in this directory.
> By default the configuration is contained in a folder named `.dir`, as you can verify with `kdb file dir:/` (_kdb file_ tells you the file where a key is stored in).
>
> For the purpose of demonstration we chose to only manipulate the verbosity of kdb.
> Note that setting `dir:/sw/elektra/kdb/#0/%/namespace` to `dir` can be handy if you want to work with configuration of an application in a certain directory.

If we now search for some key, `kdb` will behave just as if we have called it with the `-v` option.

```shell
kdb get /some/key
# RET: 10
#> got 4 keys
#> searching spec:/some/key, found: <nothing>, options: KDB_O_CALLBACK
#>     searching proc:/some/key, found: <nothing>
#>     searching dir:/some/key, found: <nothing>
#>     searching user:/some/key, found: <nothing>
#>     searching system:/some/key, found: <nothing>
#>     searching default:/some/key, found: <nothing>
#>     searching default of spec/some/key, found: <nothing>, options: KDB_O_NOCASCADING
# STDERR: Did not find key '/some/key'
```

Verbosity is not always useful because it distracts from the essential.
So we may decide that we want `kdb` to be only verbose if we are debugging it.
So let us move the default configuration to another profile:

```shell
kdb mv -r "dir:/sw/elektra/kdb/#0/%" "dir:/sw/elektra/kdb/#0/debug"
#> using common basename: dir:/sw/elektra/kdb/#0
#> key: dir:/sw/elektra/kdb/#0/%/verbose will be renamed to: dir:/sw/elektra/kdb/#0/debug/verbose
#> Will write out:
#> dir:/sw/elektra/kdb/#0/debug/verbose
```

If we now call `kdb get /some/key` it will behave non-verbose, but if we call it with the _debug_ profile `kdb get -p debug /some/key` the configuration under **/sw/elektra/kdb/#0/debug** applies.

We configured kdb only for the current directory. If we like this configuration we could move it to the system namespace, so that every user can enjoy a preconfigured _debug_ profile.

```shell
sudo kdb mv -r "dir:/sw/elektra/kdb" "system:/sw/elektra/kdb"
#> using common basename: /sw/elektra/kdb
#> key: dir:/sw/elektra/kdb/#0/%/verbose will be renamed to: system:/sw/elektra/kdb/#0/%/verbose
#> Will write out:
#> system:/sw/elektra/kdb/#0/%/verbose
```

Now every user could use the _debug_ profile with kdb.

_Cascading keys_ are keys that start with **/** and are a way of making key lookups much easier.
Let's say you want to see the configuration from the example above.
You do not need to search every namespace by yourself.
Just make a lookup for **/sw/elektra/kdb/#0/debug/verbose**, like this:

```shell
kdb get "/sw/elektra/kdb/#0/debug/verbose"
#> 1
```

When using cascading key the best key will be searched at run-time.
If you are only interested in the system key, you would use:

```shell
kdb get "system:/sw/elektra/kdb/#0/debug/verbose"
#> 0
```

Because of _cascading keys_ a user can override the behavior of the _debug_ profile by setting the corresponding keys in their _user_ namespace
(as we discussed [before](#cascading-keys)).
If a user sets _verbose_ in their user namespace to 0 they override the default behavior from the _system_ namespace.

Assuming that **sytem:/sw/elektra/kdb/#0/debug/verbose** is set to **1**, a user could override this by setting

```shell
kdb set "user:/sw/elektra/kdb/#0/debug/verbose" 0
#> Create a new key user:/sw/elektra/kdb/#0/debug/verbose with string "0"
kdb get "/sw/elektra/kdb/#0/debug/verbose"
#> 0
```

Now `kdb get -p debug /some/key` is not verbose anymore for this user.
