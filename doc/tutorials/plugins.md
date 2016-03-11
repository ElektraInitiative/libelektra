# How-To: Write a Plugin #

This file serves as a tutorial on how to write a storage plugin. Storage plugins are used by Elektra in order to store data in the Elektra Key Database
in an intelligent way. They act as a liason between configuration files and the Key Database. Storage plugins are largely responsible for
the functionality of Elektra and they allow many of its advanced features to work.

## Basics ##

First, there are a few basic points to understand about Elektra plugins. This first section will explain the basic layout of a plugin
and what various methods exists within one.

### The Interface ###

All plug-ins use the same basic interface. This interface consists of five basic functions,
[elektraPluginOpen](http://doc.libelektra.org/api/current/html/group__plugin.html#ga1a72ac76b618943677e00ed7ab50b372),
[elektraPluginGet](http://doc.libelektra.org/api/current/html/group__plugin.html#ga2f14a12b205687a31e6fd0645470ec69),
[elektraPluginSet](http://doc.libelektra.org/api/current/html/group__plugin.html#ga01dd4018e48c3a091cb03940a7a8341f),
[elektraPluginError](http://doc.libelektra.org/api/current/html/group__plugin.html#gab0f8a88ee9868fb698b4e3040a70e000), and
[elektraPluginClose](http://doc.libelektra.org/api/current/html/group__plugin.html#gaed8aeda2b2beab1b8052f8a64c601754).
The developer replaces `Plugin` with the name of their plugin. So in the case of my plugin, the names of these functions would be
`elektraLineOpen()`, `elektraLineGet()`, `elektraLineSet()`, `elektraLineError()`, and `elektraLineClose()`.
Additionally, there is one more function called
[ELEKTRA_PLUGIN_EXPORT](http://doc.libelektra.org/api/current/html/group__plugin.html#gabe78724d2d477eef39997fd9b85bff16),
where once again `Plugin` should be replaced with the name of the plug-in, this time in lower-case. So for my line plugin this function would be
`ELEKTRA_PLUGIN_EXPORT(line)`.

The KDB relies on the first five functions for interacting with configuration files stored in the key database.
Calls for kdbGet() and kdbClose() will call the functions elektraPluginGet() and elektraPluginClose() respectively for the
plugin that was used to mount the configuration data. kdbSet() calls elektraPluginSet() but also elektraPluginError() when an error occurs.
elektraPluginOpen() is called before the first call to elektraPluginGet() or elektraPluginSet(). These functions serve different purposes
that allow the plug-in to work:

- elektraPluginOpen() is designed to allow each plug-in to do initialization if necessary.
- elektraPluginGet() is designed to turn information from a configuration file into a usable KeySet, this is technically the only function that is REQUIRED in a plug-in.
- elektraPluginSet() is designed to store the information from the keyset back into a configuration file.
- elektraPluginError() is designed to allow proper rollback of operations if needed and is called if any plugin fails during the set operation. This allows exception-safety.
- elektraPluginClose() is used to free resources that might be required for the plug-in.
- ELEKTRA_PLUGIN_EXPORT(Plugin) simply lets Elektra know that the plug-in exists and what the name of the above functions are.

Most simply put: most plug-ins consist of five major functions, `elektraPluginOpen()`, `elektraPluginClose()`, `elektraPluginGet()`, `elektraPluginSet()`,
and `ELEKTRA_EXPORT_PLUGIN(Plugin)`.

Because remembering all these functions can be cumbersome, we provide a skeleton plugin in order to easily create a new plugin.
The skeleton plugin is called "template" and a new plugin can be created by calling the
[copy-template script](/scripts/copy-template) .
For example for my plugin I called `../../scripts/copy-template line` from within the plugins directory. Afterwards two
important things are left to be done:

- remove all functions (and their exports) from the plugin that are not needed. For example not every plugin actually makes use of the `elektraPluginOpen()` function.
- provide a basic contract as described above

After these two steps your plugin is ready to be compiled, installed and mounted for the first time. Have a look at
[How-To: kdb mount](http://community.libelektra.org/wp/?p=31)




## Contract ##

In Elektra, multiple plugins form a backend. If every plugin would do
whatever it likes to do, there would be chaos and backends would be
unpredictable.

To avoid this situation, plugins export a so called *contract*. In this
contract the plugin states how nicely it will behave and what other
plugins can depend on.

### Writing a Contract ###

Because the contracts also contain information for humans, these parts
are written in a README.md files of the plugins. To make the contracts
machine-readable, the following CMake command exists:

	generate_readme(pluginname)

It will generate a readme_plugginname.c (in the build-directory) out of the
README.md of the plugin''s source directory.

But prefer to use

	add_plugin(pluginname)

where the readme (among many other things) are already done for you.
More details about how to write the CMakeLists.txt will be discussed
later in the tutorial.


### Content of README.md ###

The first lines must look like:

- infos = Information about YAIL plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/recommends = rebase directoryvalue comment type
- infos/description = JSON using YAIL

The information of these parts are limited to a single line.
Only for the description an unlimited amount of lines can be used (until
the end of the file).

For the meaning (semantics) of those entries, please refer to [contract
specification](/doc/CONTRACT.ini).

The already said generate_readme will produce a list of Keys using the
information in README.md. It would look like (for the third key):

		keyNew ("system/elektra/modules/yajl/infos/licence",
			KEY_VALUE, "BSD", KEY_END),

## Including readme_pluginname.c ##

In your plugin, specifically in your elektraPluginGet()
implementation, you have to return the contract whenever configuration
below system/elektra/modules/plugin is requested:

	if (!strcmp (keyName(parentKey), "system/elektra/modules/plugin"))
	{
		KeySet *moduleConfig = elektraPluginContract();
		ksAppend(returned, moduleConfig);
		ksDel(moduleConfig);
		return 1;
	}

The elektraPluginContract() is a method implemented by the plug-in developer
containing the parts of the contract not specified in README.md.
An example of this function (taken from the yajl plugin):

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

It basically only contains the symbols to be exported (that are
dependent on your functions to be available) and the plugin version
information that is always defined to the macro PLUGINVERSION.

As already said, readme_yourplugin.c is generated in the binary directory,
so make sure that your CMakeLists.txt contains (prefer to use add_plugin
where this is already done correctly):

	include_directories (${CMAKE_CURRENT_BINARY_DIR})



## CMake ##

For every plugin you have to write a CMakeLists.txt. If your plugin has
no dependencies, you can jump this section. The full documentation of
`add_plugin` is available [here](/cmake/Modules/LibAddPlugin.cmake).

In order to understand how to write the CMakeLists.txt, you need to know that
the same file is included multiple times for different reasons.

1. The first time, only the name of plugins and directories are enquired.
    In this phase, only the `add_plugin` should be executed.
2. The second time (if the plugin is actually requested), the CMakeLists.txt
    is used to detect if all dependencies are actually available.

This means that in the first time, only the `add_plugin` should be executed
and in the second time the detection code together with `add_plugin`.

So that you can distinguish the first and second phase, the variable `DEPENDENCY_PHASE`
is set to `ON` iff you should find for all needed cmake packages. You should avoid
to search for packages otherwise, because this would:

- clutter the output
- introduce more variables into the CMakeCache which are irrelevant for the user
- maybe even find libraries in wrong versions which are incompatible to what other
  plugins need

So usually you would have:

	if (DEPENDENCY_PHASE)
		find_package (LibXml2)
		if (LIBXML2_FOUND)
			# add testdata, testcases...
		else ()
			remove_plugin (xmltool "libxml2 not found")
		endif ()
	endif ()

So if you are in the second phase (`DEPENDENCY_PHASE`), you will search for all
dependencies, in this case `LibXml2`. If all dependencies are satisfied, you add
everything needed for the plugin, except the plugin itself.
This happens after `endif ()`:

	add_plugin (xmltool
		SOURCES
			...
		LINK_LIBRARIES
			${LIBXML2_LIBRARIES}
		DEPENDENCIES
			${LIBXML2_FOUND}
		)

Important is that you pass the information which packages are found as boolean.
The plugin will actually be added iff all of the `DEPENDENCIES` are true.

Note that no code should be outside of `if (DEPENDENCY_PHASE)`
thus it would be executed twice otherwise. The only exception is
`add_plugin` which *must* be called twice to successfully add a plugin.

If your plugin makes use of [compilation variants](/doc/tutorials/compilation-variants.md)
you should also read the information there.



## Coding ##

This section will focus on an overview of the kind of code you would use to develop a plugin. It gives examples from real plugins
and should serve as a rough guide of how to write a storage plugin that can read and write configuration data into the Elektra
KeySet.

### elektraPluginGet ###

`elektraPluginGet` is the function responsible for turning information from a file into a usable KeySet.
This function usually differs pretty greatly between each plug-in. This function should be of type int, it returns 0 on success or
another number on an error. The function will take in a Key, usually called `parentKey` which contains a string containing the path
to the file that is mounted. For instance, if you run the command `kdb mount /etc/linetest system/linetest line` then `keyString(parentKey)`
should be equal to `/etc/linetest`. At this point, you generally want to open the file so you can begin saving it into keys.
Here is the trickier part to explain. Basically, at this point you will want to iterate through the file and create keys and store string values
inside of them according to what your plug-in is supposed to do. I will give a few examples of different plug-ins to better explain.

The line plug-in was written to read files into a KeySet line by line using the newline character as a delimiter and naming the keys by their line
number such as `#1`, `#2`, .. `#\_22` for a file with 22 lines. So once I open the file given by `parentKey`, every time as I read a line I create a new key,
let's call it new_key using dupKey(parentKey). Then I set new_keys's name to lineNN (where NN is the line number) using `keyAddBaseName` and
store the string value of the line into the key using `keySetString`. Once the key is initialized, I append it to the KeySet that was passed into the
elektraPluginGet function, let's call it returned for now, using `ksAppendKey(return, new_key)`. Now the KeySet will contain `new_key` with the
name `#N` properly saved where it should be according to the `kdb mount` command (in this case, `system/linetest/#N`), and a string value
equal to the contents of that line in the file. The line plug-in repeats these steps as long as it hasn't reached end of file, thus saving the whole file
into a KeySet line by line.

The simpleini plug-in works similarly, but it parses for ini files instead of just line-by-line. At their most simple level, ini files are in the format of
`name=value` with each pair taking one line. So for this plug-in, it makes a lot of sense to name each Key in the KeySet by the string to the left
of the `=` sign and store the value into each key as a string. For instance, the name of the key would be `name` and `keyGetString(name)`
would return `value`.

As you may have noticed, simpleini and line plug-ins work very similarly. However, they just parse the files differently. The simpleini plug-in parses
the file in a way that is more natural to ini file (setting the key's name to the left side of the equals sign and the value to the right side of the equals sign).
The `elektraPluginGet` function is the heart of a storage plug-in, its what allows Elektra to store configurations in it's database. This function isn't
just run when a file is first mounted, but whenever a file gets updated, this function is run to update the Elektra Key Database to match.

### elektraPluginSet ###

We also gave a brief overview of `elektraPluginSet` function. This function is basically the opposite of `elektraPluginGet`. Where `elektraPluginGet`
reads information from a file into the Elektra Key Database, `elektraPluginSet` writes information from the database back into the mounted file.

First have a look at the signature of `elektraLineSet`:

`elektraLineSet(Plugin *handle ELEKTRA_UNUSED, KeySet *toWrite, Key *parentKey)`

Lets start with the most important parameters, the KeySet and the `parentKey`. The KeySet supplied is the KeySet that is going to be persisted in
the file. In our case it would contain the Keys representing the lines. The `parentKey` is the topmost Key of the KeySet and serves several purposes.
First, it contains the filename of the destination file as its value. Second, errors and warnings can be emitted via the parentKey. We will discuss
error handling in more detail later. The Plugin handle can be used to persist state information in a threadsafe way with `elektraPluginSetData`.
As our plugin is not stateful and therefore does not use the handle, it is marked as unused in order to suppress compiler warnings.

Basically the implementation of `elektraLineSet` can be described with the following pseudocode:

	open the file
	if (error)
	{
		ELEKTRA_SET_ERROR(74, parentKey, keyString(parentKey));
	}
	for each key
	{
		write the key value together with a newline
	}
	close the file

The full-blown code can be found at [line plugin](http://libelektra.org/tree/master/src/plugins/line/line.c)

As you can see, all `elektraLineSet` does is open a file, take each Key from the KeySet (remember they are named `#1`, `#2` ... `#_22`) in order,
and write each key as it's own line in the file. Since we don't care about the name of the Key in this case (other than for order), we just write
the value of `keyString` for each Key as a new line in the file. That's it. Now, each time the mounted KeySet is modified, `elektraPluginSet` will
be called and the mounted file will be updated.

#### ELEKTRA_SET_ERROR ####

We haven't discussed `ELEKTRA_SET_ERROR` yet. Because Elektra is a library, printing errors to stderr wouldn't be a good idea. Instead, errors
and warnings can be appended to a key in the form of metadata. This is what `ELEKTRA_SET_ERROR` does. Because the parentKey always exists
even if a critical error occurs, we append the error to the `parentKey`. The first parameter is an id specifying the general error that occurred.
A listing of existing errors together with a short description and a categorization can be found at
[error specification](https://github.com/ElektraInitiative/libelektra/blob/master/src/error/specification).
The third parameter can be used to provide additional information about the error. In our case we simply supply the filename of the file that
caused the error. The kdb tools will interpret this error and print it in a pretty way. Notice that this can be used in any plugin function where the
parentKey is available.

### elektraPluginOpen and elektraPluginClose ###

The `elektraPluginOpen` and `elektraPluginClose` functions are not commonly used for storage plug-ins, but they can be useful and are worth
reviewing. `elektraPluginOpen` function runs before `elektraPluginGet` and is useful to do initialization if necessary for the plug-in. On the other
hand `elektraPluginClose` is run after other functions of the plug-in and can be useful for freeing up resources.

### ELEKTRA_PLUGIN_EXPORT ###

The last function, one that is always needed in a plug-in, is `ELEKTRA_PLUGIN_EXPORT`. This functions is responsible for letting Elektra know that
the plug-in exists and which methods it implements. The code from the line plugin is a good example and pretty self-explanatory:

	Plugin *ELEKTRA_PLUGIN_EXPORT(line)
	{
		return elektraPluginExport("line",
		ELEKTRA_PLUGIN_GET, &elektraLineGet,
		ELEKTRA_PLUGIN_SET, &elektraLineSet,
		ELEKTRA_PLUGIN_END);
	}


For further information see [the API documentation](http://doc.libelektra.org/api/current/html/group__plugin.html).
