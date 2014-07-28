# Contract #

In Elektra multiple plugins form a backend. If every plugin would do
whatever it likes to do, there would be chaos and backends would be
unpredictable.

To avoid this situation, plugins export a so called *contract*. In this
contract the plugin states how nicely it will behave and what other
plugins can depend on.

## Writing a Contract ##

Because the contracts also contain information for humans, these parts
are written in a README.md files of the plugins. To make the contracts
machine-readable, the following CMake command exists:

	generate_readme(pluginname)

It will generate a readme_plugginname.c (in the build-directory) out of the
README.md of the plugin''s source directory.


## Content of README.md ##

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

In your plugin, specifically in your elektraYourpluginGet()
implementation, you have to return the contract whenever configuration
below system/elektra/modules/yourplugin is requested:

	if (!strcmp (keyName(parentKey), "system/elektra/modules/yajl"))
	{
		KeySet *moduleConfig = elektraYourpluginContract();
		ksAppend(returned, moduleConfig);
		ksDel(moduleConfig);
		return 1;
	}

The elektraYourpluginContract() is also a method implemented by you
containing the parts of the contract not specified in README.md.
It may look like:

	static inline KeySet *elektraYourpluginContract()
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
so make sure that your CMakeLists.txt contains:

	include_directories (${CMAKE_CURRENT_BINARY_DIR})
