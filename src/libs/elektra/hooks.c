/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbinternal.h>

/**
 * Uninitializes and frees all hooks in the passed KDB handle.
 *
 * @param kdb the KDB handle where the hooks should be freed
 * @param errorKey the key which holds errors and warnings which were issued
 */
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

/**
 * Extracts the config for a single plugin from the contract
 *
 * The config must be below system:/elektra/contract/mountglobal/<pluginName> and will be moved to below user:/.
 * It also must contain the key system:/elektra/contract/mountglobal/<pluginName>.
 *
 * @param pluginName The name of the plugin for which the config will be extracted
 * @param contract The contract from which the config is extracted
 */
static KeySet * getPluginConfigFromContract (const char * pluginName, const KeySet * contract)
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

			// only handle config for the specified plugin
			// we might be able to replace this check by modifying the key mountContractRoot,
			// but I just copied this function from the previous global plugins implementation
			// and it works for now.
			if (strcmp (pluginName, pluginNameOfConfig) != 0)
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

	KeySet * config = getPluginConfigFromContract (pluginName, contract);

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

/**
 * Initializes the hooks stored in the passed KDB handle.
 * If the handle already contains initialized hooks, they will be reinitialized, including unloading and loading of their plugins.
 * Parameters @p config and @p contract will be used to determine which hooks to populate.
 *
 * @param kdb the KDB instance where the hooks should be initialized
 * @param config KeySet containing the current config in @p system:/elektra namespace
 * @param modules the current list of loaded modules
 * @param contract the contract passed to @p kdbOpen
 * @param errorKey the key which holds errors and warnings which were issued
 * @return 0 on success, -1 on failure
 */
int initHooks (KDB * kdb, const KeySet * config, KeySet * modules, const KeySet * contract, Key * errorKey)
{
	bool existingError = ksLookupByName (keyMeta (errorKey), "meta:/error", 0) != NULL;

	if (!existingError)
	{
		// set a dummy value to block errors
		// any errors that occur will be converted into warnings
		keySetMeta (errorKey, "meta:/error", "blocked");
	}

	freeHooks (kdb, errorKey);

	if (isGoptsEnabledByContract (contract) &&
	    initHooksGopts (kdb, loadPlugin ("gopts", kdb->global, modules, contract, errorKey), errorKey) != 0)
	{
		goto error;
	}

	if (!existingError)
	{
		// remove dummy error again
		keySetMeta (errorKey, "meta:/error", NULL);
	}

	return 0;

error:
	freeHooks (kdb, errorKey);

	if (!existingError)
	{
		// remove dummy error again
		keySetMeta (errorKey, "meta:/error", NULL);
	}

	return -1;
}
