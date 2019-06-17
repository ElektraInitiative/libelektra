# How-To: Write a Plugin

This file serves as a tutorial on how to write a storage plugin (which includes all information to write filter plugins).

## Types of Plugins

- [Storage plugins](storage-plugins.md) are used by Elektra in order to store data in the Elektra Key Database
  in an intelligent way. They act as a liaison between configuration files and the Key Database. Storage plugins are largely responsible for
  the functionality of Elektra and they allow many of its advanced features to work.
  These plugins act as sources and destinations of configuration settings.
- Filter plugins are simpler than storage plugins.
  They receive configuration settings in the same way as storage plugins but they do not have the responsibility to serialize the configuration
  settings to configuration files.
- Resolver plugins are more complicated and not covered by this tutorial.

## Basics

First, there are a few basic points to understand about Elektra plugins. This first section will explain the basic layout of a plugin
and what various methods exists within one.

### The Interface

All plugins use the same basic interface. This interface consists of five basic functions:

- [`elektraPluginOpen`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga23c2eb3584e38a4d494eb8f91e5e3d8d),
- [`elektraPluginGet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gacb69f3441c6d84241b4362f958fbe313),
- [`elektraPluginSet`](https://doc.libelektra.org/api/current/html/group__plugin.html#gae65781a1deb34efc79c8cb9d9174842c),
- [`elektraPluginError`](https://doc.libelektra.org/api/current/html/group__plugin.html#gad74b35f558ac7c3262f6069c5c47dc79), and
- [`elektraPluginClose`](https://doc.libelektra.org/api/current/html/group__plugin.html#ga1236aefe5b2baf8b7bf636ba5aa9ea29)

. The developer replaces `Plugin` with the name of their plugin. So in the case of the line plugin, the names of these functions would be
`elektraLineOpen()`, `elektraLineGet()`, `elektraLineSet()`, `elektraLineError()`, and `elektraLineClose()`.
Additionally, there is one more function called
[ELEKTRA_PLUGIN_EXPORT](https://doc.libelektra.org/api/current/html/group__plugin.html#ga8dd092048e972a3f0c9c9f54eb41576e),
where once again `Plugin` should be replaced with the name of the plugin, this time in uppercase. So for the line plugin this function would be
`ELEKTRA_PLUGIN_EXPORT(line)`.
The developer may also define `elektraPluginCheckConf()` if configuration validation at mount-time is desired.

The KDB relies on the first five functions for interacting with configuration files stored in the key database.
Calls to `kdbGet()` and `kdbClose()` will call the functions `elektraPluginGet()` and `elektraPluginClose()` respectively for the
plugin that was used to mount the configuration data. `kdbSet()` calls `elektraPluginSet()` but also `elektraPluginError()` when an error occurs.
`elektraPluginOpen()` is called before the first call to `elektraPluginGet()` or `elektraPluginSet()`. These functions serve different purposes
that allow the plugin to work:

- `elektraPluginOpen()` is designed to allow each plugin to do initialization if necessary.
- `elektraPluginGet()` is designed to turn information from a configuration file into a usable `KeySet`, this is technically the only function that is **required** in a plugin.
- `elektraPluginSet()` is designed to store the information from the keyset back into a configuration file.
- `elektraPluginError()` is designed to allow proper rollback of operations if needed and is called if any plugin fails during the set operation.
  This is not needed for storage plugins as the resolver already takes care to unlink the configuration files in such situations.
- `elektraPluginClose()` is used to free resources that might be required for the plugin.
- `ELEKTRA_PLUGIN_EXPORT` simply lets Elektra know that the plugin exists and what the name of the above functions are.

Most simply put: most plugins consist of five major functions, `elektraPluginOpen()`, `elektraPluginClose()`, `elektraPluginGet()`, `elektraPluginSet()`,
and `ELEKTRA_EXPORT_PLUGIN`.

Because remembering all these functions can be cumbersome, we provide a skeleton plugin in order to easily create a new plugin.
The skeleton plugin is called [`template`](/src/plugins/template/) and a new plugin can be created by calling the
[copy-template script](/scripts/copy-template) .
For example, the author of the [line plugin](/src/plugins/line/) used the command `scripts/copy-template line` to create the initial version of the plugin. Afterwards two
important things are left to be done:

- remove all functions (and their exports) from the plugin that are not needed. For example not every plugin actually makes use of the `elektraPluginOpen()` function.
- provide a basic contract as described above

After these two steps your plugin is ready to be compiled, installed and mounted for the first time. Have a look at
[How-To: kdb mount](mount.md)

#### C++ Based Plugins

If you want to use C++ instead of C for plugin development you can use [`copy-template`](/scripts/copy-template) to create a plugin based
on [`cpptemplate`](/src/plugins/cpptemplate/). For example, to create a new plugin called `pluginbaby` use the command:

```sh
scripts/copy-template -p pluginbaby
```

.

## Contract

In Elektra, multiple plugins form a backend. If every plugin would do
whatever it likes to do, there would be chaos and backends would be
unpredictable.

To avoid this situation, plugins export a so called _contract_. In this
contract the plugin states how nicely it will behave and what other
plugins can depend on.

### Writing a Contract

Because the contracts also contain information for humans, these parts
are written in a `README.md` files of the plugins. To make the contracts
machine-readable, the following CMake command exists:

```cmake
generate_readme(pluginname)
```

It will generate a `readme_plugginname.c` (in the build-directory) out of the
`README.md` of the plugin’s source directory.

But prefer to use

```cmake
add_plugin(pluginname)
```

where the generation of the readme (among many other things) are already done for you.
More details about how to write the `CMakeLists.txt` will be discussed
later in the tutorial.

The `README.md` will be used by:

- the build system (`-DPLUGINS=`), e.g. to exclude experimental plugins (`infos/status`)
- the mount tool, e.g. to correctly place and order plugins
- to know dependencies between plugin and what metadata they process

### Content of `README.md`

The first lines must look like:

```md
- infos = Information about YAJL plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/json
- infos/needs = directoryvalue
- infos/recommends = rebase comment type
- infos/placements = getstorage setstorage
- infos/status = maintained coverage unittest
- infos/description = JSON using YAJL
```

Every of these line represents a clause of the contract.
All these clauses need to be present for every plugin.

The information of clauses are limited to a single line, starting with
`-` (so that the file renders nicely in Markdown), followed by the clause
itself separated by `=`.
Only for the description an unlimited amount of lines can be
used (until the end of the file).

For the meaning (semantics) of these clauses, please refer to [contract specification](/doc/CONTRACT.ini).
The only difference for filter plugins is that their `infos/provides` and `infos/placements` are different.

The already mentioned `generate_readme` will produce a list of Keys using the
information in `README.md`. It would look like (for the third key):

```c
keyNew ("system/elektra/modules/yajl/infos/licence",
        KEY_VALUE, "BSD", KEY_END);
```

## Including `readme_pluginname.c`

In your plugin, specifically in your `elektraPluginGet()`
implementation, you have to return the contract whenever configuration
below `system/elektra/modules/plugin` is requested:

```c
if (!strcmp (keyName(parentKey), "system/elektra/modules/plugin"))
{
	KeySet *moduleConfig = elektraPluginContract();
	ksAppend(returned, moduleConfig);
	ksDel(moduleConfig);
	return 1;
}
```

The `elektraPluginContract()` is a method implemented by the plugin developer
containing the parts of the contract not specified in `README.md`.
An example of this function (taken from the [`yajl`](/src/plugins/yajl/) plugin):

```c
static inline KeySet *elektraYajlContract()
{
	return ksNew (30,
	keyNew ("system/elektra/modules/yajl",
		KEY_VALUE, "yajl plugin waits for your orders", KEY_END),
	keyNew ("system/elektra/modules/yajl/exports", KEY_END),
	keyNew ("system/elektra/modules/yajl/exports/get",
		KEY_FUNC, elektraYajlGet,
		KEY_END),
	keyNew ("system/elektra/modules/yajl/exports/set",
		KEY_FUNC, elektraYajlSet,
		KEY_END),
#include "readme_yourplugin.c"
	keyNew ("system/elektra/modules/yajl/infos/version",
		KEY_VALUE, PLUGINVERSION, KEY_END),
	keyNew ("system/elektra/modules/yajl/config", KEY_END),
	keyNew ("system/elektra/modules/yajl/config/",
		KEY_VALUE, "system",
		KEY_END),
	keyNew ("system/elektra/modules/yajl/config/below",
		KEY_VALUE, "user",
		KEY_END),
	KS_END);
}
```

It basically only contains the symbols to be exported (these symbols
depend on the functions the plugin provides) and the plugin version
information that is always defined by the macro `PLUGINVERSION`.

As already said, `readme_yourplugin.c` is generated in the binary directory,
so make sure that your `CMakeLists.txt` contains (prefer to use `add_plugin`
where this is already done correctly):

```cmake
include_directories (${CMAKE_CURRENT_BINARY_DIR})
```

## CMake

For every plugin you have to write a `CMakeLists.txt`. If your plugin has
no dependencies, you can skip this section. The full documentation of
`add_plugin` is available [here](/cmake/Modules/LibAddPlugin.cmake).

In order to understand how to write the `CMakeLists.txt`, you need to know that
the same file is included multiple times for different reasons.

1. The first time, only the name of plugins and directories are enquired.
   In this phase, only the `add_plugin` should be executed.
2. The second time (if the plugin is actually requested), the `CMakeLists.txt`
   is used to detect if all dependencies are actually available.

This means that in the first time, only the `add_plugin` should be executed
and in the second time the detection code together with `add_plugin`.

So that you can distinguish the first and second phase, the variable `DEPENDENCY_PHASE`
is set to `ON` iff you should search for all needed CMake packages. You should avoid
to search for packages otherwise, because this would:

- clutter the output
- introduce more variables into the `CMakeCache` which are irrelevant for the user
- maybe even find libraries in wrong versions which are incompatible to what other
  plugins need

So usually you would have:

```cmake
if (DEPENDENCY_PHASE)
	find_package (LibXml2 QUIET)
	if (LIBXML2_FOUND)
		# add testdata, test cases...
	else ()
		remove_plugin (xmltool "libxml2 not found")
	endif ()
endif ()
```

So if you are in the second phase (`DEPENDENCY_PHASE`), you will search for all
dependencies, in this case `LibXml2`. If all dependencies are satisfied, you add
everything needed for the plugin, except the plugin itself.
This happens after `endif ()`:

```cmake
add_plugin (xmltool
	SOURCES
		...
	LINK_LIBRARIES
		${LIBXML2_LIBRARIES}
	DEPENDENCIES
		${LIBXML2_FOUND}
	)
```

Important is that you pass the information which packages are found as boolean.
The plugin will actually be added iff all of the `DEPENDENCIES` are true.

Note that no code should be outside of `if (DEPENDENCY_PHASE)`. It would be executed twice otherwise. The only exception is
`add_plugin` which _must_ be called twice to successfully add a plugin.

> Please note that the parameters passed to `add_plugin` need to be constant between all invocations.
> Some `find_package` cache their variables, others do not, which might lead to toggling variables.
> To avoid problems, create a variable containing all `LINK_LIBRARIES` or `DEPENDENCIES` within `DEPENDENCY_PHASE`.

If your plugin makes use of [compilation variants](/doc/tutorials/compilation-variants.md)
you should also read the information there.

## Coding

This section will focus on an overview of the kind of code you would use to develop a plugin. It gives examples from real plugins
and should serve as a rough guide on how to write a storage plugin that can read and write configuration data into an Elektra
`KeySet`.

### `elektraPluginGet`

`elektraPluginGet` is the function responsible for turning information from a file into a usable `KeySet`.
This function usually differs pretty greatly between each plugin. This function should be of type `int`, it returns either `1` or on `0` on success.

- `1`: The function was successful (`ELEKTRA_PLUGIN_STATUS_SUCCESS`).
- `0`: The function was successful and the given keyset/configuration was **not changed** (`ELEKTRA_PLUGIN_STATUS_NO_UPDATE`).

Any other return value indicates an error (`ELEKTRA_PLUGIN_STATUS_ERROR`). The function will take in a `Key`, usually called `parentKey` which contains a string containing the path
to the file that is mounted. For instance, if you run the command `kdb mount /etc/linetest system/linetest line` then `keyString(parentKey)`
should be equal to `/etc/linetest`. At this point, you generally want to open the file so you can begin saving it into keys.
Here is the trickier part to explain. Basically, at this point you will want to iterate through the file and create keys and store string values
inside of them according to what your plugin is supposed to do. I will give a few examples of different plugins to better explain.

The line plugin was written to read files into a `KeySet` line by line using the newline character as a delimiter and naming the keys by their line
number such as `#1`, `#2`, .. `#_22` for a file with 22 lines. So once I open the file given by `parentKey`, every time as I read a line I create a new key,
let's call it `new_key` using `dupKey(parentKey)`. Then I set `new_key`'s name to `lineNN` (where NN is the line number) using `keyAddBaseName` and
store the string value of the line into the key using `keySetString`. Once the key is initialized, I append it to the `KeySet` that was passed into the
`elektraPluginGet` function, let's call it `returned` for now, using `ksAppendKey(returned, new_key)`. Now the `KeySet` will contain `new_key` with the
name `#N` properly saved where it should be according to the `kdb mount` command (in this case, `system/linetest/#N`), and a string value
equal to the contents of that line in the file. The line plugin repeats these steps as long as it hasn't reached end of file, thus saving the whole file
into a `KeySet` line by line.

The `simpleini` plugin works similarly, but it parses for `ini` files instead of just line-by-line. At their most simple level, `ini` files are in the format of
`name=value` with each pair taking one line. So for this plugin, it makes a lot of sense to name each `Key` in the `KeySet` by the string to the left
of the `=` sign and store the value into each key as a string. For instance, the name of the key would be `name` and `keyGetString(name)`
would return `value`.

As you may have noticed, `simpleini` and line plugins work very similarly. However, they just parse the files differently. The `simpleini` plugin parses
the file in a way that is more natural to `ini` file (setting the key's name to the left side of the equals sign and the value to the right side of the equals sign).
The `elektraPluginGet` function is the heart of a storage plugin, it’s what allows Elektra to store configurations in its database. This function isn't
just run when a file is first mounted, but whenever a file gets updated, this function is run to update the Elektra Key Database to match.

### `elektraPluginSet`

We also give a brief overview of the `elektraPluginSet` function. This function is basically the opposite of `elektraPluginGet`. Where `elektraPluginGet`
reads information from a file into the Elektra Key Database, `elektraPluginSet` writes information from the database back into the mounted file.

First have a look at the signature of `elektraLineSet`:

```c
int elektraLineSet(Plugin *handle ELEKTRA_UNUSED, KeySet *toWrite, Key *parentKey);
```

Lets start with the most important parameters, the `KeySet` and the `parentKey`. The `KeySet` supplied is the `KeySet` that is going to be persisted in
the file. In our case it would contain the Keys representing the lines. The `parentKey` is the topmost `Key` of the `KeySet` and serves several purposes.
First, it contains the filename of the destination file as its value. Second, errors and warnings can be emitted via the `parentKey`. We will discuss
error handling in more detail later. The Plugin handle can be used to persist state information in a thread-safe way with `elektraPluginSetData`.
As our plugin is not stateful and therefore does not use the handle, it is marked as unused in order to suppress compiler warnings.

Basically the implementation of `elektraLineSet` can be described with the following pseudocode:

```c
// open the file
if (error)
{
	ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
}
for (/* each key */)
{
	// write the key value together with a newline
}
// close the file
```

The full-blown code can be found at [line plugin](https://master.libelektra.org/src/plugins/line/line.c).

As you can see, all `elektraLineSet` does is open a file, take each `Key` from the `KeySet` (remember they are named `#1`, `#2` ... `#_22`) in order,
and write each key as its own line in the file. Since we don't care about the name of the `Key` in this case (other than for order), we just write
the value of `keyString` for each `Key` as a new line in the file. That's it. Now, each time the mounted `KeySet` is modified, `elektraPluginSet` will
be called and the mounted file will be updated.

#### `ELEKTRA_SET_ERROR`

We haven't discussed `ELEKTRA_SET_ERROR` yet. Because Elektra is a library, printing errors to stderr wouldn't be a good idea. Instead, errors
and warnings can be appended to a key in the form of metadata. This is what `ELEKTRA_SET_ERROR` does. Because the parentKey always exists
even if a critical error occurs, we write the error to the parentKey. The error does not necessarily have to be in a configuration.
If there are multiple errors in a configuration, only the first occurrence will be written to the metadata of the `parentKey`.

The first parameter of `ELEKTRA_SET_ERROR` is an id specifying the general error that occurred.
A listing of existing errors together with a short description and a categorization can be found at
[error specification](https://github.com/ElektraInitiative/libelektra/blob/master/src/error/specification).
The third parameter can be used to provide additional information about the error. In our case we simply supply the filename of the file that
caused the error. The kdb tools will interpret this error and print it in a pretty way. Notice that this can be used in any plugin function where the
parentKey is available.

### `elektraPluginOpen` and `elektraPluginClose`

The `elektraPluginOpen` and `elektraPluginClose` functions are not commonly used for storage plugins, but they can be useful and are worth
reviewing. `elektraPluginOpen` function runs before `elektraPluginGet` and is useful to do initialization if necessary for the plugin. On the other
hand `elektraPluginClose` is run after other functions of the plugin and can be useful for freeing up resources.

### `elektraPluginCheckConf`

The `elektraPluginCheckConf` function may be used for validation of the plugin configuration during mount-time. The signature of the function is:

```c
int elektraLineCheckConfig (Key * errorKey, KeySet * conf);
```

The configuration of the plugin is provided as `conf`. The function may report an error or warnings using the `errorKey` and the return value.

The following convention was established for the return value of `elektraPluginCheckConf`:

- `0`: The configuration was OK and has not been changed
- `1`: The configuration has been changed and now it is OK
- `-1`: The configuration was not OK and could not be fixed. An error has to be set to errorKey.

The following example demonstrates how to limit the length of the values within the plugin configuration to 3 characters.

```c
int elektraLineCheckConfig (Key * errorKey, KeySet * conf)
{
	Key * cur;
	ksRewind (conf);
	while ((cur = ksNext (conf)) != 0)
	{
		const char * value = keyString (cur);
		if (strlen (value) > 3)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_VALUE_LENGTH, errorKey,
					    "value %s is more than 3 characters long",
					    value);
			return -1; // The configuration was not OK and could not be fixed
		}
	}
	return 0; // The configuration was OK and has not been changed
}
```

The `elektraPluginCheckConf` function is exported via the plugin's contract. The following example demonstrates how to export the `checkconf` function (see section [Contract](#contract) for further details):

```c
keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkconf", KEY_FUNC, elektraLineCheckConfig, KEY_END),
```

Within the `checkconf` function all of the plugin configuration values should be validated.
Errors should be reported via Elektra's error handling mechanism (see section [ELEKTRA_SET_ERROR](#elektra_set_error) for further details).
If `checkconf` encounters a configuration value, that is not strictly invalid but can not be parsed by the plugin (e.g. a parameter which is not part of the plugin configuration), then a warning should be appended to `errorKey`, using `ELEKTRA_ADD_WARNING`.

### `ELEKTRA_PLUGIN_EXPORT`

A function that is always needed in a plugin, is `ELEKTRA_PLUGIN_EXPORT`. This functions is responsible for letting Elektra know that
the plugin exists and which methods it implements. The code from the line plugin is a good example and pretty self-explanatory:

```c
Plugin *ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport("line",
	ELEKTRA_PLUGIN_GET, &elektraLineGet,
	ELEKTRA_PLUGIN_SET, &elektraLineSet,
	ELEKTRA_PLUGIN_END);
}
```

For further information see [the API documentation](https://doc.libelektra.org/api/current/html/group__plugin.html).

### `elektraPluginGetGlobalKeySet`

In order to enable communication between plugins which is more complex than what can be done with metadata, Elektra provides a global keyset which plugins can read from and modify.

The keyset is initialized and closed by a KDB handle and can be accessed by all plugins of a single handle except for plugins created manually (e.g. with `elektraPluginOpen`). It is not shared between different KDB handles.

It can be accessed by calling the `elektraPluginGetGlobalKeySet` function, which returns a handle to the global keyset.

Plugins using the global keyset are responsible for cleaning up the parts of the keyset they no longer need.

## Note on Direct Method Calls via External Integrations

Some applications want to call Elektra methods directly via native access.
A `KeySet` is a data structure over which functions can iterate. If you want to start again from to first element,
you have to explicitly call `rewind()` to set the internal pointer to the start.
Any plugin expects the passed `KeySet` to be **rewinded**.
