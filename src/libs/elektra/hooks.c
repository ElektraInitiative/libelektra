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

static Plugin * loadPlugin (const char * pluginName,KeySet * modules, Key * errorKey)
{
	Key * openKey = keyDup (errorKey, KEY_CP_ALL);

	Plugin * plugin = elektraPluginOpen (pluginName, modules, ksNew (0, KS_END), openKey);

	if (!plugin)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Could not load plugin '%s'", pluginName);
		keyCopyAllMeta (errorKey, openKey);
		keyDel (openKey);
		return NULL;
	}

	return plugin;
}

static bool isGoptsEnabledByContract (const KeySet * contract)
{
	KeySet * dupContract = ksDup (contract); // We need to duplicate because contract is const, and ksLookupByName doesn't take const
	bool isEnabled = ksLookupByName (dupContract, "system:/elektra/contract/mountglobal/gopts", 0) != NULL;
	ksDel (dupContract);

	return isEnabled;
}

int initHooks (KDB * kdb, KeySet * modules, const KeySet * contract, Key * errorKey)
{
	freeHooks (kdb, errorKey);

	if (isGoptsEnabledByContract (contract) && !initHooksGopts (kdb, loadPlugin ("gopts", modules, errorKey)))
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Hook for 'gopts' enabled but no plugin found");
	}

	return 0;
}
