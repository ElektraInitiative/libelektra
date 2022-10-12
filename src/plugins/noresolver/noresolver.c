/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "noresolver.h"
#include <kdblogger.h>
#include <string.h>

/**
 * @retval 1 on success (Relative path)
 * @retval 0 on success (Absolute path)
 * @retval never -1 (success guaranteed)
 */
int elektraNoresolverCheckFile (const char * filename)
{
	if (filename[0] == '/') return 0;

	return 1;
}

static KeySet * elektraNoresolverModules (void)
{
	return ksNew (
		50,
		keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "", KEY_VALUE,
			"" ELEKTRA_PLUGIN_NAME " plugin waits for your orders", KEY_END),
		keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
		keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get", KEY_FUNC, elektraNoresolverGet, KEY_END),
		keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set", KEY_FUNC, elektraNoresolverSet, KEY_END),
		keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/error", KEY_FUNC, elektraNoresolverError, KEY_END),
		keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/commit", KEY_FUNC, elektraNoresolverCommit, KEY_END),
		keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkfile", KEY_FUNC, elektraNoresolverCheckFile, KEY_END),
#include ELEKTRA_README
		keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

// TODO: remove, handled by backend
int elektraNoresolverGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{

	Key * root = keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME, KEY_END);

	if (keyCmp (root, parentKey) == 0 || keyIsBelow (root, parentKey) == 1)
	{
		keyDel (root);
		KeySet * info = elektraNoresolverModules ();
		ksAppend (returned, info);
		ksDel (info);
		return 1;
	}
	keyDel (root);
	KeySet * config = elektraPluginGetConfig (handle);
	Key * pathKey = ksLookupByName (config, "/path", KDB_O_NONE);
	if (pathKey) keySetString (parentKey, keyString (pathKey));

	if (!strcmp (keyString (ksLookupByName (config, "/assume/unchanged", 0)), "1"))
	{
		// always return 0, except the first time
		uintptr_t nr = (uintptr_t) elektraPluginGetData (handle);
		if (nr == 1)
		{
			ELEKTRA_LOG ("assume config is unchanged");
			return 0;
		}
		elektraPluginSetData (handle, (void *) 1);
	}

	ELEKTRA_LOG ("assume config is changed");
	return 1; /* success */
}

int elektraNoresolverSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * pathKey = ksLookupByName (config, "/path", KDB_O_NONE);
	if (pathKey) keySetString (parentKey, keyString (pathKey));

	return 1; /* success */
}

int elektraNoresolverError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

int elektraNoresolverCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("noresolver",
		ELEKTRA_PLUGIN_GET,	&elektraNoresolverGet,
		ELEKTRA_PLUGIN_SET,	&elektraNoresolverSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraNoresolverError,
		ELEKTRA_PLUGIN_COMMIT,	&elektraNoresolverCommit,
		ELEKTRA_PLUGIN_END);
}

