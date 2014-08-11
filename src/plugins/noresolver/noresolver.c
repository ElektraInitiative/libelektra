#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include "noresolver.h"

int elektraNoresolverCheckFile(const char * f ELEKTRA_UNUSED)
{
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
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos",
			KEY_VALUE, "All information you want to know are in keys below", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/author",
			KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/licence",
			KEY_VALUE, "BSD", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/description",
			KEY_VALUE, "The " ELEKTRA_PLUGIN_NAME " just does nothing instead of resolving file names.\n" ,KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/provides",
			KEY_VALUE, "resolver", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/placements",
			KEY_VALUE, "rollback getresolver setresolver commit", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/needs",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
}

int elektraNoresolverGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
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
	return elektraPluginExport("noresolver",
		ELEKTRA_PLUGIN_GET,	&elektraNoresolverGet,
		ELEKTRA_PLUGIN_SET,	&elektraNoresolverSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraNoresolverError,
		ELEKTRA_PLUGIN_END);
}

