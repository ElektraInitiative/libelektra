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
int mountOpen (KDB * kdb, KeySet * config, KeySet * modules, Key * errorKey)
{
	Key * root;
	Key * cur;

	ksRewind (config);
	root = ksLookupByName (config, "system/elektra/mountpoints", KDB_O_CREATE);

	int ret = 0;
	while ((cur = ksNext (config)) != 0)
	{
		if (keyIsDirectlyBelow (root, cur) == 1)
		{
			KeySet * cut = ksCut (config, cur);
			Backend * backend = backendOpen (cut, modules, kdb->global, errorKey);

			if (!backend)
			{
				ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Could not create missing backend");
				ret = -1;
				continue;
			}

			if (!backend->mountpoint)
			{
				ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Backend has no mount point");
				ret = -1;
				backendClose (backend, errorKey);
				continue;
			}

			if (mountBackend (kdb, backend, errorKey) == -1)
			{
				ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Mounting of backend failed");
				ret = -1;
				/* mountBackend modified the refcounter. */
				backend->refcounter = 1;
				backendClose (backend, errorKey);
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
int mountDefault (KDB * kdb, KeySet * modules, int inFallback, Key * errorKey)
{
	// open the defaultBackend the first time
	kdb->defaultBackend = backendOpenDefault (modules, kdb->global, KDB_DB_FILE, errorKey);
	kdb->initBackend = 0;

	if (!kdb->defaultBackend)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Could not (re)open default backend");
		return -1;
	}

	if (!inFallback)
	{
		/* Reopen the init Backend for fresh user experience (update issue) */
		kdb->initBackend = backendOpenDefault (modules, kdb->global, KDB_DB_INIT, errorKey);

		if (!kdb->initBackend)
		{
			ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Could not (re)open init backend");
			return -1;
		}
	}

	Key * key = 0;
	Backend * backend = 0;

	for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ++ns)
	{
		switch (ns)
		{
		case KEY_NS_SPEC:
			key = keyNew ("spec", KEY_VALUE, "default", KEY_END);
			backend = mountGetBackend (kdb, key);
			if (backend != kdb->defaultBackend)
			{
				/* It does not matter that spec is not reachable anymore */
				keyDel (key);
			}
			else
			{
				/* User is reachable, so append that to split */
				splitAppend (kdb->split, backend, key, 2);
			}
			break;
		case KEY_NS_DIR:
			key = keyNew ("dir", KEY_VALUE, "default", KEY_END);
			backend = mountGetBackend (kdb, key);
			if (backend != kdb->defaultBackend)
			{
				/* It does not matter that dir is not reachable anymore */
				keyDel (key);
			}
			else
			{
				/* Dir is reachable, so append that to split */
				splitAppend (kdb->split, backend, key, 2);
			}
			break;
		case KEY_NS_SYSTEM:
			/* We want system/elektra still reachable
			 * through default backend.
			 * First check if it is still reachable.
			 */
			if (inFallback)
			{
				key = keyNew (KDB_SYSTEM_ELEKTRA, KEY_END);
				backend = mountGetBackend (kdb, key);
				keyDel (key);
				if (backend != kdb->defaultBackend)
				{
					/* It is not reachable, mount it */
					mountBackend (kdb, kdb->defaultBackend, errorKey);
					/*mountBackend will set refcounter*/
					++kdb->defaultBackend->refcounter;
					kdb->split->syncbits[kdb->split->size - 1] = 2;
				}
				else
				{
					/* Lets add the reachable default backend to split.
					   Note that it is not possible that system/elektra has the default
					   backend, but system has not. */
					splitAppend (kdb->split, backend, keyNew ("system", KEY_VALUE, "default", KEY_END), 2);
				}
			}
			else
			{
				/* We want system/elektra still reachable
				 * through bootstrap backend. */
				mountBackend (kdb, kdb->initBackend, errorKey);
				/*mountBackend will set refcounter*/
				++kdb->initBackend->refcounter;
				kdb->split->syncbits[kdb->split->size - 1] = 2;

				key = keyNew ("system", KEY_VALUE, "default", KEY_END);
				backend = mountGetBackend (kdb, key);
				if (backend != kdb->defaultBackend)
				{
					/* It does not matter that system is not reachable anymore */
					keyDel (key);
				}
				else
				{
					/* System is reachable, so append that to split */
					splitAppend (kdb->split, backend, key, 2);
				}
			}
			break;
		case KEY_NS_USER:
			key = keyNew ("user", KEY_VALUE, "default", KEY_END);
			backend = mountGetBackend (kdb, key);
			if (backend != kdb->defaultBackend)
			{
				/* It does not matter that user is not reachable anymore */
				keyDel (key);
			}
			else
			{
				/* User is reachable, so append that to split */
				splitAppend (kdb->split, backend, key, 2);
			}
			break;
		case KEY_NS_EMPTY:
		case KEY_NS_PROC:
		case KEY_NS_NONE:
		case KEY_NS_META:
		case KEY_NS_CASCADING:
			break;
		}
	}

	return 0;
}

KeySet * elektraMountGlobalsGetConfig (Key * cur, KeySet * global)
{
	// putting together the plugins configuration KeySet.
	Key * sysConfigCutKey = keyDup (cur);
	keyAddBaseName (sysConfigCutKey, "system");
	Key * usrConfigCutKey = keyDup (cur);
	keyAddBaseName (usrConfigCutKey, "user");
	KeySet * sysConfigKS = ksCut (global, sysConfigCutKey);
	KeySet * usrConfigKS = ksCut (global, usrConfigCutKey);
	KeySet * renamedSysConfig = elektraRenameKeys (sysConfigKS, "system");
	KeySet * renamedUsrConfig = elektraRenameKeys (usrConfigKS, "user");
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
	Key * openKey = keyDup (errorKey);

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
		if (!(*plugin) && !elektraStrCmp (pluginName, "cache") && !ksLookupByName (system, "system/elektra/cache/enabled", 0))
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
		keyDel (refKey);
	}

	keyCopyAllMeta (errorKey, openKey);
	keyDel (openKey);
	return 1;
}

KeySet * elektraDefaultGlobalConfig (KeySet * keys)
{
	KeySet * config = ksNew (
		24, keyNew ("system/elektra/globalplugins", KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit/user", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit/user/placements", KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit/user/placements/error", KEY_VALUE, "prerollback postrollback", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit/user/placements/get", KEY_VALUE,
			"pregetstorage procgetstorage postgetstorage", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit/user/placements/set", KEY_VALUE, "presetstorage precommit postcommit",
			KEY_END),
#ifndef __MINGW32__
		keyNew ("system/elektra/globalplugins/postcommit/user/plugins", KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit/user/plugins/#0", KEY_VALUE, "spec", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit/user/plugins/#0/placements", KEY_VALUE, "spec", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit/user/plugins/#0/placements/get", KEY_VALUE, "postgetstorage", KEY_END),
		keyNew ("system/elektra/globalplugins/postcommit/user/plugins/#0/placements/set", KEY_VALUE, "presetstorage", KEY_END),
#endif
		keyNew ("system/elektra/globalplugins/postgetcleanup", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/postgetstorage", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/postgetcache", KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/globalplugins/postrollback", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/precommit", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/pregetstorage", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/pregetcache", KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/globalplugins/prerollback", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/presetcleanup", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/presetstorage", KEY_VALUE, "list", KEY_END),
		keyNew ("system/elektra/globalplugins/procgetstorage", KEY_VALUE, "list", KEY_END), KS_END);

	Key * cacheEnabled = ksLookupByName (keys, "system/elektra/cache/enabled", 0);
	if (!cacheEnabled || (cacheEnabled && !elektraStrCmp (keyString (cacheEnabled), "1")))
	{
		ksAppendKey (config, keyNew ("system/elektra/globalplugins/postgetcache", KEY_VALUE, "cache", KEY_END));
		ksAppendKey (config, keyNew ("system/elektra/globalplugins/pregetcache", KEY_VALUE, "cache", KEY_END));
	}

	return config;
}

int mountGlobals (KDB * kdb, KeySet * keys, KeySet * modules, Key * errorKey)
{
	int retval = 0;
	Key * root = ksLookupByName (keys, "system/elektra/globalplugins", 0);
	KeySet * system = ksDup (keys);
	if (!root)
	{
		ELEKTRA_LOG ("no global configuration, assuming spec as default");
		KeySet * tmp = keys;
		keys = elektraDefaultGlobalConfig (keys);
		ksDel (tmp);
		root = ksHead (keys);
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
	Key * cur;

	root = ksLookupByName (modules, "system/elektra/modules", 0);

	if (!root)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "No root key found for modules");
		return -1;
	}

	KeySet * alreadyMounted = ksNew (5, KS_END);
	ssize_t oldSize = 0;

	while ((cur = ksNext (modules)) != 0)
	{
		Backend * backend = backendOpenModules (modules, kdb->global, errorKey);

		if (!backend)
		{
			// error already set in errorKey
			continue;
		}

		ksAppendKey (alreadyMounted, backend->mountpoint);
		if (ksGetSize (alreadyMounted) == oldSize)
		{
			// we already mounted that before
			backendClose (backend, errorKey);
			continue;
		}
		++oldSize;
		mountBackend (kdb, backend, errorKey);
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
	Backend * backend = backendOpenVersion (kdb->global, errorKey);
	mountBackend (kdb, backend, errorKey);

	return 0;
}

/**
 * Mounts a backend into the trie.
 *
 * @pre user must pass correctly allocated backend
 * @post sets reference counter of backend
 *
 * @warning in case of init and default backends, the reference counter needs to
 * be modified *after* calling mountBackend.
 *
 * @param kdb the handle to work with
 * @param backend the backend to mount
 * @param errorKey the key used to report warnings
 * @retval -1 on failure
 * @retval 1 on success
 * @ingroup mount
 */
int mountBackend (KDB * kdb, Backend * backend, Key * errorKey ELEKTRA_UNUSED)
{

	char * mountpoint;
	/* 20 is enough for any of the combinations below. */
	mountpoint = elektraMalloc (keyGetNameSize (backend->mountpoint) + 20);

	/* Note that you must set the refcounter to the number of insertions
	   into the trie */

	if (!strcmp (keyName (backend->mountpoint), ""))
	{
		/* Default backend */
		sprintf (mountpoint, "system/elektra/");
		kdb->trie = trieInsert (kdb->trie, mountpoint, backend);
		splitAppend (kdb->split, backend, keyNew ("system/elektra/", KEY_VALUE, "default", KEY_END), 0);
		backend->refcounter = 1;
	}
	else if (!strcmp (keyName (backend->mountpoint), "/"))
	{
		/* Root backend */
		backend->refcounter = 0;
		for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ++ns)
		{
			switch (ns)
			{
			case KEY_NS_SPEC:
				sprintf (mountpoint, "spec%s", keyName (backend->mountpoint));
				kdb->trie = trieInsert (kdb->trie, mountpoint, backend);
				splitAppend (kdb->split, backend, keyNew ("spec", KEY_VALUE, "root", KEY_END), 2);
				++backend->refcounter;
				break;
			case KEY_NS_DIR:
				sprintf (mountpoint, "dir%s", keyName (backend->mountpoint));
				kdb->trie = trieInsert (kdb->trie, mountpoint, backend);
				splitAppend (kdb->split, backend, keyNew ("dir", KEY_VALUE, "root", KEY_END), 2);
				++backend->refcounter;
				break;
			case KEY_NS_USER:
				sprintf (mountpoint, "user%s", keyName (backend->mountpoint));
				kdb->trie = trieInsert (kdb->trie, mountpoint, backend);
				splitAppend (kdb->split, backend, keyNew ("user", KEY_VALUE, "root", KEY_END), 2);
				++backend->refcounter;
				break;
			case KEY_NS_SYSTEM:
				sprintf (mountpoint, "system%s", keyName (backend->mountpoint));
				kdb->trie = trieInsert (kdb->trie, mountpoint, backend);
				splitAppend (kdb->split, backend, keyNew ("system", KEY_VALUE, "root", KEY_END), 2);
				++backend->refcounter;
				break;
			case KEY_NS_PROC:
			case KEY_NS_EMPTY:
			case KEY_NS_NONE:
			case KEY_NS_META:
			case KEY_NS_CASCADING:
				break;
			}
		}
	}
	else if (keyName (backend->mountpoint)[0] == '/')
	{
		/* Cascading Backend */
		backend->refcounter = 0;
		for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ++ns)
		{
			switch (ns)
			{
			case KEY_NS_DIR:
				sprintf (mountpoint, "dir%s/", keyName (backend->mountpoint));
				kdb->trie = trieInsert (kdb->trie, mountpoint, backend);
				splitAppend (kdb->split, backend, keyNew (mountpoint, KEY_VALUE, keyString (backend->mountpoint), KEY_END),
					     2);
				++backend->refcounter;
				break;
			case KEY_NS_USER:
				sprintf (mountpoint, "user%s/", keyName (backend->mountpoint));
				kdb->trie = trieInsert (kdb->trie, mountpoint, backend);
				splitAppend (kdb->split, backend, keyNew (mountpoint, KEY_VALUE, keyString (backend->mountpoint), KEY_END),
					     2);
				++backend->refcounter;
				break;
			case KEY_NS_SYSTEM:
				sprintf (mountpoint, "system%s/", keyName (backend->mountpoint));
				kdb->trie = trieInsert (kdb->trie, mountpoint, backend);
				splitAppend (kdb->split, backend, keyNew (mountpoint, KEY_VALUE, keyString (backend->mountpoint), KEY_END),
					     2);
				++backend->refcounter;
				break;
			case KEY_NS_SPEC:
			// excluded on purpose because mounting spec is a separate step
			// (see specmount)
			case KEY_NS_EMPTY:
			case KEY_NS_PROC:
			case KEY_NS_NONE:
			case KEY_NS_META:
			case KEY_NS_CASCADING:
				break;
			}
		}
	}
	else
	{
		/* Common single mounted backend */
		sprintf (mountpoint, "%s/", keyName (backend->mountpoint));
		kdb->trie = trieInsert (kdb->trie, mountpoint, backend);
		splitAppend (kdb->split, backend, keyDup (backend->mountpoint), 0);
		backend->refcounter = 1;
	}

	elektraFree (mountpoint);

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
Key * key = keyNew ("system/template");
KDB * handle = kdbOpen();
Key *mountpoint=0;
mountpoint=kdbGetMountpoint(handle, key);

printf("The backend I am using is %s mounted in %s\n",
	keyValue(mountpoint),
	keyName(mountpoint));
kdbClose (handle);
keyDel (key);
 * @endcode
 *
 *
 * @param handle is the data structure, where the mounted directories are saved.
 * @param where the key, that should be looked up.
 * @return the mountpoint associated with the key
 * @ingroup mount
 */
Key * mountGetMountpoint (KDB * handle, const Key * where)
{
	Backend * backend_handle;

	backend_handle = mountGetBackend (handle, where);
	if (!backend_handle)
	{
		return 0;
	}

	return backend_handle->mountpoint;
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
 * @param key the key, that should be looked up.
 * @return the backend handle associated with the key
 * @ingroup mount
 */
Backend * mountGetBackend (KDB * handle, const Key * key)
{
	if (!key || !strcmp (keyName (key), "")) return handle->defaultBackend;

	Backend * ret = trieLookup (handle->trie, key);
	if (!ret) return handle->defaultBackend;
	return ret;
}
