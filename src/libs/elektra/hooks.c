/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbinternal.h>

static Plugin * loadPlugin (const char * pluginName, KeySet * global, KeySet * modules, const KeySet * contract, Key * errorKey);

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
		kdb->hooks.gopts.plugin = NULL;
		kdb->hooks.gopts.get = NULL;
	}

	if (kdb->hooks.spec.plugin != NULL)
	{
		elektraPluginClose (kdb->hooks.spec.plugin, errorKey);
		kdb->hooks.spec.plugin = NULL;
		kdb->hooks.spec.copy = NULL;
		kdb->hooks.spec.remove = NULL;
	}

	if (kdb->hooks.sendNotification != NULL)
	{
		SendNotificationHook * hook = kdb->hooks.sendNotification;
		while (hook != NULL)
		{
			elektraPluginClose (hook->plugin, errorKey);
			hook->plugin = NULL;
			hook->get = NULL;
			hook->set = NULL;

			SendNotificationHook * old = hook;
			hook = hook->next;
			elektraFree (old);
		}

		kdb->hooks.sendNotification = NULL;
	}

	if (kdb->hooks.record.plugin != NULL)
	{
		elektraPluginClose (kdb->hooks.record.plugin, errorKey);
		kdb->hooks.record.record = NULL;
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

	if ((kdb->hooks.gopts.get = (kdbHookGoptsGetPtr) getFunction (plugin, "hook/gopts/get", errorKey)) == NULL)
	{
		elektraPluginClose (plugin, errorKey);
		return -1;
	}

	kdb->hooks.gopts.plugin = plugin;

	return 0;
}

static int initHooksSpec (KDB * kdb, Plugin * plugin, Key * errorKey)
{
	if (!plugin)
	{
		return -1;
	}

	kdb->hooks.spec.copy = (kdbHookSpecCopyPtr) getFunction (plugin, "hook/spec/copy", errorKey);
	kdb->hooks.spec.remove = (kdbHookSpecRemovePtr) getFunction (plugin, "hook/spec/remove", errorKey);

	if (kdb->hooks.spec.copy == NULL || kdb->hooks.spec.remove == NULL)
	{
		elektraPluginClose (plugin, errorKey);
		return -1;
	}

	kdb->hooks.spec.plugin = plugin;

	return 0;
}

static int initHooksRecord (KDB * kdb, Plugin * plugin, Key * errorKey)
{
	if (!plugin)
	{
		return -1;
	}

	kdb->hooks.record.record = (kdbHookRecordPtr) getFunction (plugin, "hook/record/record", errorKey);

	if (kdb->hooks.record.record == NULL)
	{
		elektraPluginClose (plugin, errorKey);
		return -1;
	}

	kdb->hooks.record.plugin = plugin;

	return 0;
}

static KeySet * getSendNotificationHooksEnforcedByContract (const KeySet * contract)
{
	KeySet * returned = ksNew (0, KS_END);

	KeySet * dupContract = ksDup (contract);

	if (ksLookupByName (dupContract, "system:/elektra/contract/mountglobal/internalnotification", 0) != NULL)
	{
		ksAppendKey (returned, keyNew ("system:/elektra/hook/notification/send/plugins/byContractInternalnotification", KEY_VALUE,
					       "internalnotification", KEY_END));
	}

	ksDel (dupContract);

	return returned;
}

static int initHooksSendNotifications (KDB * kdb, const KeySet * config, KeySet * modules, const KeySet * contract, Key * errorKey)
{
	SendNotificationHook * lastHook = kdb->hooks.sendNotification;

	KeySet * pluginsToLoad = getSendNotificationHooksEnforcedByContract (contract);
	ksAppend (pluginsToLoad, config);

	Key * pluginsKey = keyNew ("system:/elektra/hook/notification/send/plugins", KEY_END);

	// Iterate through the config and find all direct children below system:/elektra/hook/notification/send/plugins
	// This is defined as an array, so the elements should be like
	//   - system:/elektra/hook/notification/send/plugins/#0
	//   - system:/elektra/hook/notification/send/plugins/#1
	// We actually don't check for the # to be present, so it will currently work on all keys directly below.
	for (elektraCursor end, it = ksFindHierarchy (pluginsToLoad, pluginsKey, &end); it < end; it++)
	{
		Key * cur = ksAtCursor (pluginsToLoad, it);
		if (keyIsDirectlyBelow (pluginsKey, cur) == 0)
		{
			continue;
		}

		const char * pluginName = keyString (cur);

		// Check whether this plugin has already been loaded
		// TODO: This makes this function have a runtime complexity of O(n^2).
		//       Use a better algorithm, e.g. a hashmap.
		bool alreadyLoaded = false;
		SendNotificationHook * tmp = kdb->hooks.sendNotification;
		while (tmp != NULL)
		{
			if (strcmp (pluginName, tmp->plugin->name) == 0)
			{
				alreadyLoaded = true;
				break; // cancel this while loop
			}

			tmp = tmp->next;
		}

		if (alreadyLoaded)
		{
			continue;
		}

		Plugin * plugin = loadPlugin (pluginName, kdb->global, modules, contract, errorKey);

		if (!plugin)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "SendNotification plugin %s not found, referenced by key %s",
							   pluginName, keyName (cur));
			continue;
		}

		kdbHookSendNotificationGetPtr getPtr =
			(kdbHookSendNotificationGetPtr) getFunction (plugin, "hook/notification/send/get", errorKey);
		kdbHookSendNotificationSetPtr setPtr =
			(kdbHookSendNotificationSetPtr) getFunction (plugin, "hook/notification/send/set", errorKey);

		if (getPtr == NULL && setPtr == NULL)
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (
				errorKey,
				"SendNotification plugin %s exports neither 'hook/notification/send/get' nor 'hook/notification/send/set'",
				pluginName);
			elektraPluginClose (plugin, errorKey);
			continue;
		}

		SendNotificationHook * hook = elektraMalloc (sizeof (SendNotificationHook));

		hook->next = NULL;
		hook->plugin = plugin;
		hook->get = getPtr;
		hook->set = setPtr;

		if (lastHook == NULL)
		{
			kdb->hooks.sendNotification = hook;
			lastHook = hook;
		}
		else
		{
			lastHook->next = hook;
			lastHook = hook;
		}
	}

	ksDel (pluginsToLoad);
	keyDel (pluginsKey);

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
			ksDel (pluginConfig);

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
	KeySet * config = getPluginConfigFromContract (pluginName, contract);

	Plugin * plugin = elektraPluginOpen (pluginName, modules, config, errorKey);

	if (!plugin)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Could not load plugin '%s'", pluginName);
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

static bool isSpecEnabledByConfig (const KeySet * config ELEKTRA_UNUSED)
{
	// TODO: check for system:/elektra/hook/spec/enabled or system:/elektra/hook/spec/disabled or something else ... TBD
	//       See this discussion: https://github.com/ElektraInitiative/libelektra/issues/4499
	return true;
}

/**
 * This method looks for the hook plugin 'internalnotification'
 *
 * @param kdb the KDB instance in which to search
 * @return NULL if not loaded, pointer to the plugin otherwise
 */
Plugin * elektraFindInternalNotificationPlugin (KDB * kdb)
{
	SendNotificationHook * hook = kdb->hooks.sendNotification;
	while (hook != NULL)
	{
		if (strcmp (hook->plugin->name, "internalnotification") == 0)
		{
			return hook->plugin;
		}

		hook = hook->next;
	}

	return NULL;
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

	if (isSpecEnabledByConfig (config) &&
	    initHooksSpec (kdb, loadPlugin ("spec", kdb->global, modules, contract, errorKey), errorKey) != 0)
	{
		goto error;
	}

	if (initHooksSendNotifications (kdb, config, modules, contract, errorKey))
	{
		goto error;
	}

	// No need for error handling here
	// If the plugin does not exist, recording functionality will not be used
	initHooksRecord (kdb, loadPlugin ("recorder", kdb->global, modules, contract, errorKey), errorKey);

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
