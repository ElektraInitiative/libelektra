/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include "noresolver.h"

/**
 * @retval 1 on success (Relative path)
 * @retval 0 on success (Absolute path)
 * @retval never -1 (success guaranteed)
 */
int elektraNoresolverCheckFile(const char * filename)
{
	if (filename[0] == '/') return 0;

	return 1;
}

static KeySet * elektraNoresolverModules()
{
	return ksNew (50,
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "",
			KEY_VALUE, "" ELEKTRA_PLUGIN_NAME " plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get",
			KEY_FUNC, elektraNoresolverGet,
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set",
			KEY_FUNC, elektraNoresolverSet,
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/error",
			KEY_FUNC, elektraNoresolverError,
			KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkfile",
			KEY_FUNC, elektraNoresolverCheckFile,
			KEY_END),
#include ELEKTRA_README(noresolver)
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
}

int elektraNoresolverGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey)
{

	Key *root = keyNew("system/elektra/modules/"
			ELEKTRA_PLUGIN_NAME , KEY_END);

	if (keyRel(root, parentKey) >= 0)
	{
		keyDel(root);
		KeySet *info = elektraNoresolverModules();
		ksAppend(returned, info);
		ksDel (info);
		return 1;
	}
	keyDel(root);

	/* get all keys */

	return 1; /* success */
}

int elektraNoresolverSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

int elektraNoresolverError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(noresolver)
{
	// clang-format off
	return elektraPluginExport("noresolver",
		ELEKTRA_PLUGIN_GET,	&elektraNoresolverGet,
		ELEKTRA_PLUGIN_SET,	&elektraNoresolverSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraNoresolverError,
		ELEKTRA_PLUGIN_END);
}

