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

static size_t getFunction (Plugin * plugin, const char * functionName, Key * errorKey)
{
	size_t result = elektraPluginGetFunction (plugin, functionName);

	if (result == 0)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Plugin '%s' does not implement function '%s'", plugin->name, functionName);
	}

	return result;
}

static int initHooksGopts (KDB * kdb, Plugin * plugin, Key * errorKey)
{
	if (!plugin)
	{
		return -1;
	}

	kdb->hooks.gopts.plugin = plugin;

	if ((kdb->hooks.gopts.kdbHookGoptsGet = (kdbHookGoptsGetPtr) getFunction (plugin, "hook/gopts/get", errorKey)) == NULL)
	{
		return -1;
	}

	return 0;
}

static KeySet * getPluginConfigFromContract(const char * pluginName, const KeySet * contract)
{
	KeySet * tmpContract = ksDup (contract);
	KeySet * config = ksNew (0, KS_END);

	Key * mountContractRoot = keyNew ("system:/elektra/contract/mountglobal", KEY_END);
	Key * pluginConfigRoot = keyNew ("user:/", KEY_END);

	for (elektraCursor it = ksFindHierarchy (tmpContract, mountContractRoot, NULL); it < ksGetSize (tmpContract); it++)
	{
		Key * cur = ksAtCursor (tmpContract, it);
		if (keyIsDirectlyBelow (mountContractRoot, cur) == 1)
		{
			const char * pluginNameOfConfig = keyBaseName (cur);
			if(strcmp (pluginName, pluginNameOfConfig) != 0)
			{
				break;
			}

			KeySet * pluginConfig = ksCut (tmpContract, cur);

			// increment ref count, because cur is part of pluginConfig and
			// we hold a reference to cur that is still needed (via pluginName)
			keyIncRef (cur);
			ksRename (pluginConfig, cur, pluginConfigRoot);
			ksAppend (config, pluginConfig);

			// we need to delete cur separately, because it was ksCut() from contract
			// we also need to decrement the ref count, because it was incremented above
			keyDecRef (cur);
			keyDel (cur);

			--it;
		}
	}

	keyDel (mountContractRoot);
	keyDel (pluginConfigRoot);
	ksDel (tmpContract);

	return config;
}

static Plugin * loadPlugin (const char * pluginName, KeySet * global, KeySet * modules, const KeySet * contract, Key * errorKey)
{
	Key * openKey = keyDup (errorKey, KEY_CP_ALL);

	KeySet * config = getPluginConfigFromContract(pluginName, contract);

	Plugin * plugin = elektraPluginOpen (pluginName, modules, config, openKey);

	if (!plugin)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Could not load plugin '%s'", pluginName);
		keyCopyAllMeta (errorKey, openKey);
		keyDel (openKey);
		return NULL;
	}

	plugin->global = global;

	return plugin;
}

static bool isGoptsEnabledByContract (const KeySet * contract)
{
	KeySet * dupContract = ksDup (contract); // We need to duplicate because contract is const, and ksLookupByName doesn't take const
	bool isEnabled = ksLookupByName (dupContract, "system:/elektra/contract/mountglobal/gopts", 0) != NULL;
	ksDel (dupContract);

	return isEnabled;
}

int initHooks (KDB * kdb, const KeySet * config, KeySet * modules, const KeySet * contract, Key * errorKey)
{
	freeHooks (kdb, errorKey);

	if (isGoptsEnabledByContract (contract) && initHooksGopts (kdb, loadPlugin ("gopts", kdb->global, modules, contract, errorKey), errorKey) != 0)
	{
		goto error;
	}

	return 0;

error:
	freeHooks (kdb, errorKey);
	return -1;
}
