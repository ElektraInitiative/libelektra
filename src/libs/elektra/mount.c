/**
 * @file
 *
 * @brief Internals of mount functionality.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <kdbassert.h>

#include "kdbinternal.h"


/**
 * Creates a trie from a given configuration.
 *
 * The config will be deleted within this function.
 *
 * @note mountDefault is not allowed to be executed before
 *
 * @param kdb the handle to work with
 * @param modules the current list of loaded modules
 * @param config the configuration which should be used to build up the trie.
 * @param errorKey the key used to report warnings
 * @retval -1 on failure
 * @retval 0 on success
 * @ingroup mount
 */
int mountOpen (KDB * kdb, KeySet * config, KeySet * modules, Key * errorKey)
{
	Key * root;
	Key * cur;

	ksRewind (config);
	root = ksLookupByName (config, "system:/elektra/mountpoints", KDB_O_CREATE);

	int ret = 0;
	while ((cur = ksNext (config)) != 0)
	{
		if (keyIsDirectlyBelow (root, cur) == 1)
		{
			KeySet * cut = ksCut (config, cur);
			Plugin * backend = backendOpen (cut, modules, kdb->global, errorKey);

			if (!backend)
			{
				ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Could not create missing backend");
				ret = -1;
				continue;
			}

			if (!strcmp (keyString (backendGetMountpoint (backend)), ""))
			{
				ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Backend has no mount point");
				ret = -1;
				elektraPluginClose (backend, errorKey);
				continue;
			}

			Key * mountpoint = keyNew (keyBaseName (cur), KEY_END);
			ret = mountBackend (kdb, mountpoint, backend);
			keyDel (mountpoint);
			if (ret == -1)
			{
				ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Mounting of backend failed");
				ret = -1;
				/* mountBackend modified the refcounter. */
				backend->refcounter = 1;
				elektraPluginClose (backend, errorKey);
				continue;
			}
		}
	}
	ksDel (config);

	return ret;
}


/** Reopens the default backend and mounts the default backend if needed.
 *
 * @pre Default Backend is closed. mountOpen was executed before.
 *
 * @param kdb the handle to work with
 * @param modules the current list of loaded modules
 * @param errorKey the key used to report warnings
 * @retval -1 on error
 * @retval 0 on success
 * @ingroup mount
 */
int mountDefault (KDB * kdb, KeySet * modules, Key * errorKey)
{
	// open the defaultBackend the first time
	Plugin * defaultBackend = backendOpenDefault (modules, kdb->global, KDB_DB_FILE, errorKey);
	if (defaultBackend == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Could not (re)open default backend");
		return -1;
	}
	BackendData defaultBackendData = {
		.backend = defaultBackend,
		.keys = ksNew (0, KS_END),
		.plugins = NULL,
		.definition = NULL,
		.getSize = 0,
		.initialized = false,
		.keyNeedsSync = false,
	};
	ksAppendKey (kdb->backends,
		     keyNew ("default:/", KEY_BINARY, KEY_SIZE, sizeof (BackendData), KEY_VALUE, &defaultBackendData, KEY_END));

	/* Reopen the init Backend for fresh user experience (update issue) */
	Plugin * initBackend = backendOpenDefault (modules, kdb->global, KDB_DB_INIT, errorKey);
	if (initBackend == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Could not (re)open init backend");
		return -1;
	}
	BackendData initBackendData = {
		.backend = initBackend,
		.keys = ksNew (0, KS_END),
		.plugins = NULL,
		.definition = NULL,
		.getSize = 0,
		.initialized = false,
		.keyNeedsSync = false,
	};
	ksAppendKey (kdb->backends,
		     keyNew ("system:/elektra", KEY_BINARY, KEY_SIZE, sizeof (BackendData), KEY_VALUE, &initBackendData, KEY_END));

	Key * lookupKey = keyNew ("/", KEY_END);
	for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ++ns)
	{
		Plugin * backend;
		switch (ns)
		{
		case KEY_NS_SPEC:
		case KEY_NS_DIR:
		case KEY_NS_USER:
		case KEY_NS_SYSTEM:
			keySetNamespace (lookupKey, ns);
			backend = mountGetBackend (kdb, lookupKey);
			if (backend == NULL || backend == defaultBackend)
			{
				Key * backendKey = keyDup (lookupKey, KEY_CP_NAME);
				BackendData backendData = {
					.backend = defaultBackend,
					.keys = ksNew (0, KS_END),
					.plugins = NULL,
					.definition = NULL,
					.getSize = 0,
					.initialized = false,
					.keyNeedsSync = false,
				};
				keySetBinary (backendKey, &backendData, sizeof (BackendData));
				ksAppendKey (kdb->backends, backendKey);
			}
			break;
		case KEY_NS_PROC:
		case KEY_NS_NONE:
		case KEY_NS_META:
		case KEY_NS_CASCADING:
		case KEY_NS_DEFAULT:
			break;
		}
	}

	return 0;
}

KeySet * elektraMountGlobalsGetConfig (Key * cur, KeySet * global)
{
	// putting together the plugins configuration KeySet.
	Key * sysConfigCutKey = keyDup (cur, KEY_CP_ALL);
	keyAddBaseName (sysConfigCutKey, "system");
	Key * usrConfigCutKey = keyDup (cur, KEY_CP_ALL);
	keyAddBaseName (usrConfigCutKey, "user");
	KeySet * sysConfigKS = ksCut (global, sysConfigCutKey);
	KeySet * usrConfigKS = ksCut (global, usrConfigCutKey);
	KeySet * renamedSysConfig = ksRenameKeys (sysConfigKS, "system:/");
	KeySet * renamedUsrConfig = ksRenameKeys (usrConfigKS, "user:/");
	ksDel (sysConfigKS);
	ksDel (usrConfigKS);
	keyDel (usrConfigCutKey);
	keyDel (sysConfigCutKey);
	KeySet * config = ksNew (0, KS_END);
	ksAppend (config, renamedSysConfig);
	ksAppend (config, renamedUsrConfig);
	ksDel (renamedSysConfig);
	ksDel (renamedUsrConfig);

	return config;
}

Key * elektraMountGlobalsFindPlugin (KeySet * referencePlugins, Key * cur)
{
	Key * refKey;
	Key * searchKey = keyNew ("/", KEY_END);
	keyAddBaseName (searchKey, keyString (cur));
	refKey = ksLookup (referencePlugins, searchKey, 0);
	keyDel (searchKey);
	return refKey;
}

/**
 * Loads global plugin
 *
 * @retval -1 on failure
 * @retval 0 on empty plugin name (nothing configured at given position)
 * @retval 1 on success
 */
int elektraMountGlobalsLoadPlugin (Plugin ** plugin, KeySet * referencePlugins, Key * cur, KeySet * global, KeySet * system,
				   KeySet * modules, Key * errorKey)
{
	Key * refKey = elektraMountGlobalsFindPlugin (referencePlugins, cur);
	Key * openKey = keyDup (errorKey, KEY_CP_ALL);

	if (refKey)
	{
		// plugin already loaded, just reference it
		*plugin = *(Plugin **) keyValue (refKey);
		(*plugin)->refcounter += 1;
	}
	else
	{
		KeySet * config = elektraMountGlobalsGetConfig (cur, global);
		ELEKTRA_NOT_NULL (config);
		// config holds a newly allocated KeySet
		const char * pluginName = keyString (cur);
		if (!pluginName || pluginName[0] == '\0')
		{
			keyDel (openKey);
			ksDel (config);
			return 0;
		}

		// loading the new plugin
		*plugin = elektraPluginOpen (pluginName, modules, config, openKey);
		if (!(*plugin) && !elektraStrCmp (pluginName, "cache") && !ksLookupByName (system, "system:/elektra/cache/enabled", 0))
		{
			keyDel (openKey);
			return 0;
		}
		else if (!(*plugin))
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Could not load plugin '%s'", pluginName);
			keyCopyAllMeta (errorKey, openKey);
			keyDel (openKey);
			return -1;
		}

		// saving the plugin reference to avoid having to load the plugin multiple times
		refKey = keyNew ("/", KEY_BINARY, KEY_SIZE, sizeof (Plugin *), KEY_VALUE, &(*plugin), KEY_END);
		keyAddBaseName (refKey, keyString (cur));
		ksAppendKey (referencePlugins, refKey);
	}

	keyCopyAllMeta (errorKey, openKey);
	keyDel (openKey);
	return 1;
}

KeySet * elektraDefaultGlobalConfig (KeySet * keys)
{
	KeySet * config = ksNew (
		24, keyNew ("system:/elektra/globalplugins", KEY_VALUE, "", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user/placements", KEY_VALUE, "", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user/placements/error", KEY_VALUE, "prerollback postrollback", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user/placements/get", KEY_VALUE,
			"pregetstorage procgetstorage postgetstorage", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user/placements/set", KEY_VALUE, "presetstorage precommit postcommit",
			KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user/plugins", KEY_VALUE, "", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user/plugins/#0", KEY_VALUE, "spec", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user/plugins/#0/placements", KEY_VALUE, "spec", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user/plugins/#0/placements/get", KEY_VALUE, "postgetstorage", KEY_END),
		keyNew ("system:/elektra/globalplugins/postcommit/user/plugins/#0/placements/set", KEY_VALUE, "presetstorage", KEY_END),
		keyNew ("system:/elektra/globalplugins/postgetcleanup", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/postgetstorage", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/postgetcache", KEY_VALUE, "", KEY_END),
		keyNew ("system:/elektra/globalplugins/postrollback", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/precommit", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/pregetstorage", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/pregetcache", KEY_VALUE, "", KEY_END),
		keyNew ("system:/elektra/globalplugins/prerollback", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/presetcleanup", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/presetstorage", KEY_VALUE, "list", KEY_END),
		keyNew ("system:/elektra/globalplugins/procgetstorage", KEY_VALUE, "list", KEY_END), KS_END);

	Key * cacheEnabled = ksLookupByName (keys, "system:/elektra/cache/enabled", 0);
	if (!cacheEnabled || (cacheEnabled && !elektraStrCmp (keyString (cacheEnabled), "1")))
	{
		ksAppendKey (config, keyNew ("system:/elektra/globalplugins/postgetcache", KEY_VALUE, "cache", KEY_END));
		ksAppendKey (config, keyNew ("system:/elektra/globalplugins/pregetcache", KEY_VALUE, "cache", KEY_END));
	}

	return config;
}

int mountGlobals (KDB * kdb, KeySet * keys, KeySet * modules, Key * errorKey)
{
	int retval = 0;
	Key * root = ksLookupByName (keys, "system:/elektra/globalplugins", 0);
	KeySet * system = ksDup (keys);
	if (!root)
	{
		ELEKTRA_LOG ("no global configuration, assuming spec as default");
		KeySet * tmp = keys;
		keys = elektraDefaultGlobalConfig (keys);
		ksDel (tmp);
		root = ksAtCursor (keys, 0);
	}
	memset (kdb->globalPlugins, 0, NR_GLOBAL_POSITIONS * NR_GLOBAL_SUBPOSITIONS * sizeof (Plugin *));

	KeySet * global = ksCut (keys, root);
	Key * cur;
	KeySet * referencePlugins = ksNew (0, KS_END);
	while ((cur = ksNext (global)) != NULL)
	{
		// the cutpoints for the plugin configs are always directly below the "root", ignore everything else
		if (keyIsDirectlyBelow (root, cur) != 1) continue;

		char * placement = elektraStrDup (keyBaseName (cur));

		for (GlobalpluginPositions i = 0; i < NR_GLOBAL_POSITIONS; ++i)
		{
			if (!elektraStrCaseCmp (placement, GlobalpluginPositionsStr[i]))
			{

				ELEKTRA_LOG_DEBUG ("mounting global plugin %s to %s\n", keyString (cur), placement);

				// load plugins in implicit max once placement
				Plugin * plugin = 0;
				int mountRet =
					elektraMountGlobalsLoadPlugin (&plugin, referencePlugins, cur, global, system, modules, errorKey);

				if (mountRet == -1)
					retval = -1; // error loading plugin
				else if (mountRet == 0)
					continue; // no plugin configured here
				else
				{
					kdb->globalPlugins[i][MAXONCE] = plugin;
					// set handle to global keyset
					plugin->global = kdb->global;
				}

				// load plugins in explicit placements
				const char * placementName = keyName (cur);
				Key * placementKey = ksLookupByName (global, placementName, 0);
				KeySet * subPositions = ksCut (global, placementKey);
				Key * curSubPosition;
				while ((curSubPosition = ksNext (subPositions)) != NULL)
				{
					if (keyIsDirectlyBelow (placementKey, curSubPosition) != 1) continue;
					const char * subPlacement = keyBaseName (curSubPosition);

					for (GlobalpluginSubPositions j = 0; j < NR_GLOBAL_SUBPOSITIONS; ++j)
					{
						if (j == MAXONCE) continue;

						if (!elektraStrCaseCmp (subPlacement, GlobalpluginSubPositionsStr[j]))
						{
							Plugin * subPlugin = 0;
							int subRet = // makes ksCut on Keyset `global`!
								elektraMountGlobalsLoadPlugin (&subPlugin, referencePlugins, curSubPosition,
											       subPositions, system, modules, errorKey);
							if (subRet == -1)
								retval = -1; // error loading plugin
							else if (subRet == 0)
								continue; // no plugin configured here
							else
							{
								kdb->globalPlugins[i][j] = subPlugin;
								// set handle to global keyset
								subPlugin->global = kdb->global;
							}
						}
					}
				}
				ksDel (subPositions);
			}
		}
		elektraFree (placement);
	}
	ksDel (global);
	ksDel (keys);
	ksDel (referencePlugins);
	ksDel (system);
	return retval;
}

/** Mount all module configurations.
 *
 * @param kdb the handle to work with
 * @param modules the current list of loaded modules
 * @param errorKey the key used to report warnings
 * @ingroup mount
 * @retval -1 if not rootkey was found
 * @retval 0 otherwise
 */
int mountModules (KDB * kdb, KeySet * modules, Key * errorKey)
{
	Key * root;

	root = ksLookupByName (modules, "system:/elektra/modules", 0);

	if (!root)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "No root key found for modules");
		return -1;
	}

	KeySet * alreadyMounted = ksNew (5, KS_END);
	ssize_t oldSize = 0;

	for (elektraCursor it = ksSearch (modules, root) + 1; it < ksGetSize (modules); ++it)
	{
		if (!strcmp (keyName (ksAtCursor (modules, it)), "system:/elektra/modules/backend"))
		{
			// the backend plugin does not need its own backend
			continue;
		}

		Plugin * backend = backendOpenModules (modules, kdb->global, errorKey, it);

		if (!backend)
		{
			// error already set in errorKey
			continue;
		}

		ksAppendKey (alreadyMounted, backendGetMountpoint (backend));
		if (ksGetSize (alreadyMounted) == oldSize)
		{
			// we already mounted that before
			elektraPluginClose (backend, errorKey);
			continue;
		}
		++oldSize;
		mountBackend (kdb, ksAtCursor (modules, it), backend);
	}

	ksDel (alreadyMounted);

	return 0;
}

/** Mount the version backend
 *
 * @param kdb the handle to work with
 * @param errorKey the key used to report warnings
 * @ingroup mount
 * @retval 0 on success
 */
int mountVersion (KDB * kdb, Key * errorKey)
{
	Plugin * backend = backendOpenVersion (kdb->global, kdb->modules, errorKey);
	Key * mountpoint = keyNew (KDB_SYSTEM_ELEKTRA "/version", KEY_END);
	mountBackend (kdb, mountpoint, backend);
	keyDel (mountpoint);

	return 0;
}

/**
 * Mounts a backend into the trie.
 *
 *
 * @param kdb the handle to work with
 * @param mountpoint where the backend is mounted
 * @param backend the backend plugin to use
 * @retval -1 on failure
 * @retval 1 on success
 * @ingroup mount
 */
int mountBackend (KDB * kdb, const Key * mountpoint, Plugin * backend)
{
	if (strcmp (keyName (mountpoint), "system:/elektra") == 0)
	{
		return -1;
	}

	Key * backendKey = keyDup (mountpoint, KEY_CP_NAME);
	BackendData backendData = {
		.backend = backend,
		.keys = ksNew (0, KS_END),
		.plugins = NULL,
		.definition = NULL,
		.getSize = 0,
		.initialized = false,
		.keyNeedsSync = false,
	};
	keySetBinary (backendKey, &backendData, sizeof (BackendData));

	if (keyGetNamespace (mountpoint) == KEY_NS_CASCADING)
	{
		Key * backendKeyDir = keyDup (backendKey, KEY_CP_NAME | KEY_CP_VALUE);
		keySetNamespace (backendKeyDir, KEY_NS_DIR);
		((BackendData *) keyValue (backendKeyDir))->keys = ksNew (0, KS_END);
		ksAppendKey (kdb->backends, backendKeyDir);

		Key * backendKeyUser = keyDup (backendKey, KEY_CP_NAME | KEY_CP_VALUE);
		keySetNamespace (backendKeyUser, KEY_NS_USER);
		((BackendData *) keyValue (backendKeyUser))->keys = ksNew (0, KS_END);
		ksAppendKey (kdb->backends, backendKeyUser);

		Key * backendKeySystem = keyDup (backendKey, KEY_CP_NAME | KEY_CP_VALUE);
		keySetNamespace (backendKeySystem, KEY_NS_SYSTEM);
		((BackendData *) keyValue (backendKeySystem))->keys = ksNew (0, KS_END);
		ksAppendKey (kdb->backends, backendKeySystem);

		if (keyGetUnescapedNameSize (mountpoint) == 3)
		{
			// root key
			Key * backendKeySpec = keyDup (backendKey, KEY_CP_NAME | KEY_CP_VALUE);
			keySetNamespace (backendKeySpec, KEY_NS_SPEC);
			((BackendData *) keyValue (backendKeySpec))->keys = ksNew (0, KS_END);
			ksAppendKey (kdb->backends, backendKeySpec);
		}

		keyDel (backendKey);
	}
	else
	{
		ksAppendKey (kdb->backends, backendKey);
	}

	return 1;
}


/**
 * Lookup a mountpoint in a handle for a specific key.
 *
 * Will return a key representing the mountpoint or null
 * if there is no appropriate mountpoint e.g. its the
 * root mountpoint.
 *
 * @par Example:
 * @code
KDB * handle = kdbOpen();
Key *mountpoint=0;
mountpoint=kdbGetMountpoint(handle, "system:/template");

printf("The backend I am using is %s mounted in %s\n",
	keyValue(mountpoint),
	keyName(mountpoint));
kdbClose (handle);
 * @endcode
 *
 *
 * @param handle is the data structure, where the mounted directories are saved.
 * @param where the key name, that should be looked up.
 * @return the mountpoint associated with the key
 * @ingroup mount
 */
const Key * mountGetMountpoint (KDB * handle, Key * where)
{
	const Key * backendKey = where == NULL ? ksLookupByName (handle->backends, "default:/", 0) : ksLookup (handle->backends, where, 0);

	if (backendKey == NULL)
	{
		return ksLookupByName (handle->backends, "default:/", 0);
	}

	return backendKey;
}


/**
 * Lookup a backend handle for a specific key.
 *
 * The required canonical name is ensured by using a key as parameter,
 * which will transform the key to canonical representation.
 *
 * Will return handle when no more specific KDB could be
 * found.
 *
 * If key is 0 or invalid the default backend will be returned.
 *
 * @param handle is the data structure, where the mounted directories are saved.
 * @param where the key name, that should be looked up.
 * @return the backend handle associated with the key
 * @ingroup mount
 */
Plugin * mountGetBackend (KDB * handle, Key * where)
{
	const Key * backendKey = mountGetMountpoint (handle, where);

	if (backendKey == NULL)
	{
		return NULL;
	}

	const BackendData * backendData = keyValue (backendKey);
	return backendData->backend;
}
