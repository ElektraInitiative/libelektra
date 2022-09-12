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

#if DEBUG && defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
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
int mountOpen (ElektraKdb * kdb, ElektraKeyset * config, ElektraKeyset * modules, ElektraKey * errorKey)
{
	ElektraKey * root;
	ElektraKey * cur;

	elektraKeysetRewind (config);
	root = elektraKeysetLookupByName (config, "system:/elektra/mountpoints", ELEKTRA_KDB_O_CREATE);

	int ret = 0;
	while ((cur = elektraKeysetNext (config)) != 0)
	{
		if (elektraKeyIsDirectlyBelow (root, cur) == 1)
		{
			ElektraKeyset * cut = elektraKeysetCut (config, cur);
			Plugin * backend = backendOpen (cut, modules, kdb->global, errorKey);

			if (!backend)
			{
				ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Could not create missing backend");
				ret = -1;
				continue;
			}

			if (!strcmp (elektraKeyString (backendGetMountpoint (backend)), ""))
			{
				ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Backend has no mount point");
				ret = -1;
				elektraPluginClose (backend, errorKey);
				continue;
			}

			ElektraKey * mountpoint = elektraKeyNew (elektraKeyBaseName (cur), ELEKTRA_KEY_END);
			ret = mountBackend (kdb, mountpoint, backend);
			elektraKeyDel (mountpoint);
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
	elektraKeysetDel (config);

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
int mountDefault (ElektraKdb * kdb, ElektraKeyset * modules, ElektraKey * errorKey)
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
		.keys = elektraKeysetNew (0, ELEKTRA_KS_END),
		.plugins = NULL,
		.definition = NULL,
		.getSize = 0,
		.initialized = false,
		.keyNeedsSync = false,
	};
	elektraKeysetAppendKey (kdb->backends,
		     elektraKeyNew ("default:/", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (BackendData), ELEKTRA_KEY_VALUE, &defaultBackendData, ELEKTRA_KEY_END));

	/* Reopen the init Backend for fresh user experience (update issue) */
	Plugin * initBackend = backendOpenDefault (modules, kdb->global, KDB_DB_INIT, errorKey);
	if (initBackend == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Could not (re)open init backend");
		return -1;
	}
	BackendData initBackendData = {
		.backend = initBackend,
		.keys = elektraKeysetNew (0, ELEKTRA_KS_END),
		.plugins = NULL,
		.definition = NULL,
		.getSize = 0,
		.initialized = false,
		.keyNeedsSync = false,
	};
	elektraKeysetAppendKey (kdb->backends,
		     elektraKeyNew ("system:/elektra", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (BackendData), ELEKTRA_KEY_VALUE, &initBackendData, ELEKTRA_KEY_END));

	ElektraKey * lookupKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	for (elektraNamespace ns = ELEKTRA_NS_FIRST; ns <= ELEKTRA_NS_LAST; ++ns)
	{
		Plugin * backend;
		switch (ns)
		{
		case ELEKTRA_NS_SPEC:
		case ELEKTRA_NS_DIR:
		case ELEKTRA_NS_USER:
		case ELEKTRA_NS_SYSTEM:
			elektraKeySetNamespace (lookupKey, ns);
			backend = mountGetBackend (kdb, lookupKey);
			if (backend == NULL || backend == defaultBackend)
			{
				ElektraKey * backendKey = elektraKeyDup (lookupKey, ELEKTRA_KEY_CP_NAME);
				BackendData backendData = {
					.backend = defaultBackend,
					.keys = elektraKeysetNew (0, ELEKTRA_KS_END),
					.plugins = NULL,
					.definition = NULL,
					.getSize = 0,
					.initialized = false,
					.keyNeedsSync = false,
				};
				elektraKeySetBinary (backendKey, &backendData, sizeof (BackendData));
				elektraKeysetAppendKey (kdb->backends, backendKey);
			}
			break;
		case ELEKTRA_NS_PROC:
		case ELEKTRA_NS_NONE:
		case ELEKTRA_NS_META:
		case ELEKTRA_NS_CASCADING:
		case ELEKTRA_NS_DEFAULT:
			break;
		}
	}

	return 0;
}

ElektraKeyset * elektraMountGlobalsGetConfig (ElektraKey * cur, ElektraKeyset * global)
{
	// putting together the plugins configuration KeySet.
	ElektraKey * sysConfigCutKey = elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (sysConfigCutKey, "system");
	ElektraKey * usrConfigCutKey = elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddBaseName (usrConfigCutKey, "user");
	ElektraKeyset * sysConfigKS = elektraKeysetCut (global, sysConfigCutKey);
	ElektraKeyset * usrConfigKS = elektraKeysetCut (global, usrConfigCutKey);
	ElektraKeyset * renamedSysConfig = elektraKeysetRenameKeys (sysConfigKS, "system:/");
	ElektraKeyset * renamedUsrConfig = elektraKeysetRenameKeys (usrConfigKS, "user:/");
	elektraKeysetDel (sysConfigKS);
	elektraKeysetDel (usrConfigKS);
	elektraKeyDel (usrConfigCutKey);
	elektraKeyDel (sysConfigCutKey);
	ElektraKeyset * config = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetAppend (config, renamedSysConfig);
	elektraKeysetAppend (config, renamedUsrConfig);
	elektraKeysetDel (renamedSysConfig);
	elektraKeysetDel (renamedUsrConfig);

	return config;
}

ElektraKey * elektraMountGlobalsFindPlugin (ElektraKeyset * referencePlugins, ElektraKey * cur)
{
	ElektraKey * refKey;
	ElektraKey * searchKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (searchKey, elektraKeyString (cur));
	refKey = elektraKeysetLookup (referencePlugins, searchKey, 0);
	elektraKeyDel (searchKey);
	return refKey;
}

/**
 * Loads global plugin
 *
 * @retval -1 on failure
 * @retval 0 on empty plugin name (nothing configured at given position)
 * @retval 1 on success
 */
int elektraMountGlobalsLoadPlugin (Plugin ** plugin, ElektraKeyset * referencePlugins, ElektraKey * cur, ElektraKeyset * global, ElektraKeyset * system,
				   ElektraKeyset * modules, ElektraKey * errorKey)
{
	ElektraKey * refKey = elektraMountGlobalsFindPlugin (referencePlugins, cur);
	ElektraKey * openKey = elektraKeyDup (errorKey, ELEKTRA_KEY_CP_ALL);

	if (refKey)
	{
		// plugin already loaded, just reference it
		*plugin = *(Plugin **) elektraKeyValue (refKey);
		(*plugin)->refcounter += 1;
	}
	else
	{
		ElektraKeyset * config = elektraMountGlobalsGetConfig (cur, global);
		ELEKTRA_NOT_NULL (config);
		// config holds a newly allocated KeySet
		const char * pluginName = elektraKeyString (cur);
		if (!pluginName || pluginName[0] == '\0')
		{
			elektraKeyDel (openKey);
			elektraKeysetDel (config);
			return 0;
		}

		// loading the new plugin
		*plugin = elektraPluginOpen (pluginName, modules, config, openKey);
		if (!(*plugin) && !elektraStrCmp (pluginName, "cache") && !elektraKeysetLookupByName (system, "system:/elektra/cache/enabled", 0))
		{
			elektraKeyDel (openKey);
			return 0;
		}
		else if (!(*plugin))
		{
			ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Could not load plugin '%s'", pluginName);
			elektraKeyCopyAllMeta (errorKey, openKey);
			elektraKeyDel (openKey);
			return -1;
		}

		// saving the plugin reference to avoid having to load the plugin multiple times
		refKey = elektraKeyNew ("/", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (Plugin *), ELEKTRA_KEY_VALUE, &(*plugin), ELEKTRA_KEY_END);
		elektraKeyAddBaseName (refKey, elektraKeyString (cur));
		elektraKeysetAppendKey (referencePlugins, refKey);
	}

	elektraKeyCopyAllMeta (errorKey, openKey);
	elektraKeyDel (openKey);
	return 1;
}

ElektraKeyset * elektraDefaultGlobalConfig (ElektraKeyset * keys)
{
	ElektraKeyset * config = elektraKeysetNew (
		24, elektraKeyNew ("system:/elektra/globalplugins", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user/placements", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user/placements/error", ELEKTRA_KEY_VALUE, "prerollback postrollback", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user/placements/get", ELEKTRA_KEY_VALUE,
			"pregetstorage procgetstorage postgetstorage", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user/placements/set", ELEKTRA_KEY_VALUE, "presetstorage precommit postcommit",
			ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user/plugins", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user/plugins/#0", ELEKTRA_KEY_VALUE, "spec", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user/plugins/#0/placements", ELEKTRA_KEY_VALUE, "spec", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user/plugins/#0/placements/get", ELEKTRA_KEY_VALUE, "postgetstorage", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postcommit/user/plugins/#0/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postgetcleanup", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postgetstorage", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postgetcache", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/postrollback", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/precommit", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/pregetstorage", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/pregetcache", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/prerollback", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/presetcleanup", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/presetstorage", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END),
		elektraKeyNew ("system:/elektra/globalplugins/procgetstorage", ELEKTRA_KEY_VALUE, "list", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey * cacheEnabled = elektraKeysetLookupByName (keys, "system:/elektra/cache/enabled", 0);
	if (!cacheEnabled || (cacheEnabled && !elektraStrCmp (elektraKeyString (cacheEnabled), "1")))
	{
		elektraKeysetAppendKey (config, elektraKeyNew ("system:/elektra/globalplugins/postgetcache", ELEKTRA_KEY_VALUE, "cache", ELEKTRA_KEY_END));
		elektraKeysetAppendKey (config, elektraKeyNew ("system:/elektra/globalplugins/pregetcache", ELEKTRA_KEY_VALUE, "cache", ELEKTRA_KEY_END));
	}

	return config;
}

int mountGlobals (ElektraKdb * kdb, ElektraKeyset * keys, ElektraKeyset * modules, ElektraKey * errorKey)
{
	int retval = 0;
	ElektraKey * root = elektraKeysetLookupByName (keys, "system:/elektra/globalplugins", 0);
	ElektraKeyset * system = elektraKeysetDup (keys);
	if (!root)
	{
		ELEKTRA_LOG ("no global configuration, assuming spec as default");
		ElektraKeyset * tmp = keys;
		keys = elektraDefaultGlobalConfig (keys);
		elektraKeysetDel (tmp);
		root = elektraKeysetHead (keys);
	}
	memset (kdb->globalPlugins, 0, NR_GLOBAL_POSITIONS * NR_GLOBAL_SUBPOSITIONS * sizeof (Plugin *));

	ElektraKeyset * global = elektraKeysetCut (keys, root);
	ElektraKey * cur;
	ElektraKeyset * referencePlugins = elektraKeysetNew (0, ELEKTRA_KS_END);
	while ((cur = elektraKeysetNext (global)) != NULL)
	{
		// the cutpoints for the plugin configs are always directly below the "root", ignore everything else
		if (elektraKeyIsDirectlyBelow (root, cur) != 1) continue;

		char * placement = elektraStrDup (elektraKeyBaseName (cur));

		for (GlobalpluginPositions i = 0; i < NR_GLOBAL_POSITIONS; ++i)
		{
			if (!elektraStrCaseCmp (placement, GlobalpluginPositionsStr[i]))
			{
#if DEBUG && VERBOSE
				printf ("mounting global plugin %s to %s\n", pluginName, placement);
#endif
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
				const char * placementName = elektraKeyName (cur);
				ElektraKey * placementKey = elektraKeysetLookupByName (global, placementName, 0);
				ElektraKeyset * subPositions = elektraKeysetCut (global, placementKey);
				ElektraKey * curSubPosition;
				while ((curSubPosition = elektraKeysetNext (subPositions)) != NULL)
				{
					if (elektraKeyIsDirectlyBelow (placementKey, curSubPosition) != 1) continue;
					const char * subPlacement = elektraKeyBaseName (curSubPosition);

					for (GlobalpluginSubPositions j = 0; j < NR_GLOBAL_SUBPOSITIONS; ++j)
					{
						if (j == MAXONCE) continue;

						if (!elektraStrCaseCmp (subPlacement, GlobalpluginSubPositionsStr[j]))
						{
							Plugin * subPlugin = 0;
							int subRet =
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
				elektraKeysetDel (subPositions);
			}
		}
		elektraFree (placement);
	}
	elektraKeysetDel (global);
	elektraKeysetDel (keys);
	elektraKeysetDel (referencePlugins);
	elektraKeysetDel (system);
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
int mountModules (ElektraKdb * kdb, ElektraKeyset * modules, ElektraKey * errorKey)
{
	ElektraKey * root;
	ElektraKey * cur;

	root = elektraKeysetLookupByName (modules, "system:/elektra/modules", 0);

	if (!root)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "No root key found for modules");
		return -1;
	}

	ElektraKeyset * alreadyMounted = elektraKeysetNew (5, ELEKTRA_KS_END);
	ssize_t oldSize = 0;

	while ((cur = elektraKeysetNext (modules)) != 0)
	{
		if (!strcmp (elektraKeyName (cur), "system:/elektra/modules/backend"))
		{
			// the backend plugin does not need its own backend
			continue;
		}

		Plugin * backend = backendOpenModules (modules, kdb->global, errorKey);

		if (!backend)
		{
			// error already set in errorKey
			continue;
		}

		elektraKeysetAppendKey (alreadyMounted, backendGetMountpoint (backend));
		if (elektraKeysetGetSize (alreadyMounted) == oldSize)
		{
			// we already mounted that before
			elektraPluginClose (backend, errorKey);
			continue;
		}
		++oldSize;
		mountBackend (kdb, cur, backend);
	}

	elektraKeysetDel (alreadyMounted);

	return 0;
}

/** Mount the version backend
 *
 * @param kdb the handle to work with
 * @param errorKey the key used to report warnings
 * @ingroup mount
 * @retval 0 on success
 */
int mountVersion (ElektraKdb * kdb, ElektraKey * errorKey)
{
	Plugin * backend = backendOpenVersion (kdb->global, kdb->modules, errorKey);
	ElektraKey * mountpoint = elektraKeyNew (KDB_SYSTEM_ELEKTRA "/version", ELEKTRA_KEY_END);
	mountBackend (kdb, mountpoint, backend);
	elektraKeyDel (mountpoint);

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
int mountBackend (ElektraKdb * kdb, const ElektraKey * mountpoint, Plugin * backend)
{
	if (strcmp (elektraKeyName (mountpoint), "system:/elektra") == 0)
	{
		return -1;
	}

	ElektraKey * backendKey = elektraKeyDup (mountpoint, ELEKTRA_KEY_CP_NAME);
	BackendData backendData = {
		.backend = backend,
		.keys = elektraKeysetNew (0, ELEKTRA_KS_END),
		.plugins = NULL,
		.definition = NULL,
		.getSize = 0,
		.initialized = false,
		.keyNeedsSync = false,
	};
	elektraKeySetBinary (backendKey, &backendData, sizeof (BackendData));

	if (elektraKeyGetNamespace (mountpoint) == ELEKTRA_NS_CASCADING)
	{
		ElektraKey * backendKeyDir = elektraKeyDup (backendKey, ELEKTRA_KEY_CP_NAME | ELEKTRA_KEY_CP_VALUE);
		elektraKeySetNamespace (backendKeyDir, ELEKTRA_NS_DIR);
		((BackendData *) elektraKeyValue (backendKeyDir))->keys = elektraKeysetNew (0, ELEKTRA_KS_END);
		elektraKeysetAppendKey (kdb->backends, backendKeyDir);

		ElektraKey * backendKeyUser = elektraKeyDup (backendKey, ELEKTRA_KEY_CP_NAME | ELEKTRA_KEY_CP_VALUE);
		elektraKeySetNamespace (backendKeyUser, ELEKTRA_NS_USER);
		((BackendData *) elektraKeyValue (backendKeyUser))->keys = elektraKeysetNew (0, ELEKTRA_KS_END);
		elektraKeysetAppendKey (kdb->backends, backendKeyUser);

		ElektraKey * backendKeySystem = elektraKeyDup (backendKey, ELEKTRA_KEY_CP_NAME | ELEKTRA_KEY_CP_VALUE);
		elektraKeySetNamespace (backendKeySystem, ELEKTRA_NS_SYSTEM);
		((BackendData *) elektraKeyValue (backendKeySystem))->keys = elektraKeysetNew (0, ELEKTRA_KS_END);
		elektraKeysetAppendKey (kdb->backends, backendKeySystem);

		if (elektraKeyGetUnescapedNameSize (mountpoint) == 3)
		{
			// root key
			ElektraKey * backendKeySpec = elektraKeyDup (backendKey, ELEKTRA_KEY_CP_NAME | ELEKTRA_KEY_CP_VALUE);
			elektraKeySetNamespace (backendKeySpec, ELEKTRA_NS_SPEC);
			((BackendData *) elektraKeyValue (backendKeySpec))->keys = elektraKeysetNew (0, ELEKTRA_KS_END);
			elektraKeysetAppendKey (kdb->backends, backendKeySpec);
		}

		elektraKeyDel (backendKey);
	}
	else
	{
		elektraKeysetAppendKey (kdb->backends, backendKey);
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
const ElektraKey * mountGetMountpoint (ElektraKdb * handle, ElektraKey * where)
{
	const ElektraKey * backendKey = where == NULL ? elektraKeysetLookupByName (handle->backends, "default:/", 0) : elektraKeysetLookup (handle->backends, where, 0);

	if (backendKey == NULL)
	{
		return elektraKeysetLookupByName (handle->backends, "default:/", 0);
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
Plugin * mountGetBackend (ElektraKdb * handle, ElektraKey * where)
{
	const ElektraKey * backendKey = mountGetMountpoint (handle, where);

	if (backendKey == NULL)
	{
		return NULL;
	}

	const BackendData * backendData = elektraKeyValue (backendKey);
	return backendData->backend;
}
