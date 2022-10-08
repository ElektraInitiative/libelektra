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

static KeySet * elektraMountGlobalsGetConfig (Key * cur, KeySet * global)
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

static Key * elektraMountGlobalsFindPlugin (KeySet * referencePlugins, Key * cur)
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
static int elektraMountGlobalsLoadPlugin (Plugin ** plugin, KeySet * referencePlugins, Key * cur, KeySet * global, KeySet * system,
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

static KeySet * elektraDefaultGlobalConfig (KeySet * keys)
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
