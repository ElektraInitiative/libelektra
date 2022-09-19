/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbinternal.h>

void freeHooks (KDB * kdb, Key * errorKey)
{
	if (kdb->hooks.gopts.plugin != NULL)
	{
		elektraPluginClose (kdb->hooks.gopts.plugin, errorKey);
	}
}

static int initHooksGopts (KDB * kdb, Plugin * plugin)
{
	if (!plugin)
	{
		return -1;
	}

	kdb->hooks.gopts.plugin = plugin;
	kdb->hooks.gopts.kdbHookGoptsGet = (kdbHookGoptsGetPtr) elektraPluginGetFunction (plugin, "hooks/gopts/get");

	return 0;
}

static Plugin * loadPlugin (const char * pluginName, KeySet * config, KeySet * modules, Key * errorKey)
{
	Key * openKey = keyDup (errorKey, KEY_CP_ALL);

	Plugin * plugin = elektraPluginOpen (pluginName, modules, config, openKey);

	if (!plugin)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Could not load plugin '%s'", pluginName);
		keyCopyAllMeta (errorKey, openKey);
		keyDel (openKey);
		return NULL;
	}

	return plugin;
}

static bool isGoptsEnabledByContract (KDB * kdb)
{
	bool goptsEnabled = false;

	KeySet * ksTemp = ksDup (kdb->global);
	Key * cutPoint = keyNew ("system:/elektra/gopts", KEY_END);

	KeySet * cut = ksCut (ksTemp, cutPoint);

	goptsEnabled = ksGetSize (cut) > 0;

	keyDel (cutPoint);
	ksDel (ksTemp);
	ksDel (cut);

	return goptsEnabled;
}

int initHooks (KDB * kdb, KeySet * config, KeySet * modules, Key * errorKey)
{
	freeHooks (kdb, errorKey);

	if (isGoptsEnabledByContract (kdb) && !initHooksGopts (kdb, loadPlugin ("gopts", config, modules, errorKey)))
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Hook for 'gopts' enabled but no plugin found");
	}

	return 0;
}
