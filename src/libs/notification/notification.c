/**
 * @file
 *
 * @brief Implementation of notification functions as defined in kdbnotification.h
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "../../plugins/internalnotification/internalnotification.h"
#include <kdbassert.h>
#include <kdbease.h>
#include <kdbinternal.h>
#include <kdbioprivate.h>
#include <kdblogger.h>
#include <kdbnotification.h>

#include <stdio.h>

// TODO perhaps move to libs/elektra/io?
/**
 * Set I/O binding for asynchronous I/O operations for KDB instance.
 *
 * @param  kdb       KDB instance
 * @param  ioBinding I/O binding
 */
void elektraSetIoBinding (KDB * kdb, ElektraIoInterface * ioBinding)
{
	kdb->ioBinding = ioBinding;
}

/**
 * Get I/O binding for asynchronous I/O operations for KDB instance.
 * Returns NULL if no I/O binding was set.
 *
 * @param  kdb KDB instance
 * @return I/O binding or NULL
 */
ElektraIoInterface * elektraGetIoBinding (KDB * kdb)
{
	ELEKTRA_NOT_NULL (kdb);
	return kdb->ioBinding;
}


static void debugKeySet (KeySet * ks)
{
	Key * cur;
	ksRewind (ks);
	while ((cur = ksNext (ks)) != NULL)
	{
		printf ("debugKeySet %s = %s\n", keyName (cur), keyString (cur));
	}

	return;
}

static const size_t * getPluginFunction (Plugin * plugin, const char * name)
{
	KeySet * exports = ksNew (0, KS_END);
	Key * pk = keyNew ("system/elektra/modules", KEY_END);
	keyAddBaseName (pk, plugin->name);
	plugin->kdbGet (plugin, exports, pk);
	ksRewind (exports);
	keyAddBaseName (pk, "exports");
	keyAddBaseName (pk, name);
	const size_t * func = keyValue (ksLookup (exports, pk, 0));
	if (!func)
	{
		ELEKTRA_LOG_WARNING ("could not get function \"%s\" from plugin \"%s\"", name, plugin->name);
	}
	return func;
}

/**
 * @internal
 * Converts a placement name to an index in the globalPlugins array of
 * the internal KDB structure.
 *
 * @param  placement Placement name
 * @return           Placement index or -1 on unknown placement name
 */
int placementToPosition (char * placement)
{
	if (strcmp (placement, "prerollback") == 0)
	{
		return PREROLLBACK;
	}
	else if (strcmp (placement, "rollback") == 0)
	{
		return ROLLBACK;
	}
	else if (strcmp (placement, "postrollback") == 0)
	{
		return POSTROLLBACK;
	}
	else if (strcmp (placement, "getresolver") == 0)
	{
		return GETRESOLVER;
	}
	else if (strcmp (placement, "pregetstorage") == 0)
	{
		return PREGETSTORAGE;
	}
	else if (strcmp (placement, "getstorage") == 0)
	{
		return GETSTORAGE;
	}
	else if (strcmp (placement, "postgetstorage") == 0)
	{
		return POSTGETSTORAGE;
	}
	else if (strcmp (placement, "setresolver") == 0)
	{
		return SETRESOLVER;
	}
	else if (strcmp (placement, "postgetcleanup") == 0)
	{
		return POSTGETCLEANUP;
	}
	else if (strcmp (placement, "presetstorage") == 0)
	{
		return PRESETSTORAGE;
	}
	else if (strcmp (placement, "setstorage") == 0)
	{
		return SETSTORAGE;
	}
	else if (strcmp (placement, "presetstorage") == 0)
	{
		return PRESETSTORAGE;
	}
	else if (strcmp (placement, "setstorage") == 0)
	{
		return SETSTORAGE;
	}
	else if (strcmp (placement, "presetcleanup") == 0)
	{
		return PRESETCLEANUP;
	}
	else if (strcmp (placement, "precommit") == 0)
	{
		return PRECOMMIT;
	}
	else if (strcmp (placement, "commit") == 0)
	{
		return COMMIT;
	}
	else if (strcmp (placement, "postcommit") == 0)
	{
		return POSTCOMMIT;
	}
	else
	{
		ELEKTRA_LOG_WARNING ("unknown placement name \"%s\"", placement);
		return -1;
	}
}

/**
 * @internal
 * Convert plament name to list plugin's position type.
 *
 * The list plugin distinguishes between three position types: get, set & error.
 * Plament names are converted to one of these position types.
 *
 * @param  placement Placement name
 * @return           Placement type for list plugin or NULL on unknown placement name
 */
static char * placementToListPositionType (char * placement)
{
	if (strcmp (placement, "prerollback") == 0)
	{
		return "error";
	}
	else if (strcmp (placement, "rollback") == 0)
	{
		return "error";
	}
	else if (strcmp (placement, "postrollback") == 0)
	{
		return "error";
	}
	else if (strcmp (placement, "getresolver") == 0)
	{
		return "get";
	}
	else if (strcmp (placement, "pregetstorage") == 0)
	{
		return "get";
	}
	else if (strcmp (placement, "getstorage") == 0)
	{
		return "get";
	}
	else if (strcmp (placement, "postgetstorage") == 0)
	{
		return "get";
	}
	else if (strcmp (placement, "setresolver") == 0)
	{
		return "set";
	}
	else if (strcmp (placement, "postgetcleanup") == 0)
	{
		return "get";
	}
	else if (strcmp (placement, "presetstorage") == 0)
	{
		return "set";
	}
	else if (strcmp (placement, "setstorage") == 0)
	{
		return "set";
	}
	else if (strcmp (placement, "presetstorage") == 0)
	{
		return "set";
	}
	else if (strcmp (placement, "setstorage") == 0)
	{
		return "set";
	}
	else if (strcmp (placement, "presetcleanup") == 0)
	{
		return "set";
	}
	else if (strcmp (placement, "precommit") == 0)
	{
		return "set";
	}
	else if (strcmp (placement, "commit") == 0)
	{
		return "set";
	}
	else if (strcmp (placement, "postcommit") == 0)
	{
		return "set";
	}
	else
	{
		ELEKTRA_LOG_WARNING ("unknown placement name \"%s\"", placement);
		return NULL;
	}
}

/**
 * @internal
 * Load notification plugin.
 *
 * Uses module cache from KDB handle.
 * The plugin only needs to be closed after use.
 *
 * @param  kdb KDB handle
 * @return     Plugin handle or NULL on error
 */
static Plugin * loadNotificationPlugin (KDB * kdb)
{
	// Load required plugin
	Key * errorKey = keyNew (0);
	KeySet * moduleCache = kdb->modules; // use kdb module cache
	Plugin * plugin = elektraPluginOpen ("internalnotification", moduleCache, NULL, errorKey);

	int hasError = keyGetMeta (errorKey, "error") != NULL;
	if (!plugin || hasError)
	{
		ELEKTRA_LOG_WARNING ("elektraPluginOpen failed!\n");
		return NULL;
	}
	keyDel (errorKey);

	return plugin;
}

/**
 * @internal
 * Read placement list from plugin.
 *
 * The returned string needs to be freed.
 *
 * @param  plugin Plugin
 * @return        Space separated list of placement names
 */
static char * getPluginPlacementList (Plugin * plugin)
{
	// Get placements from plugin
	Key * pluginInfo = keyNew ("system/elektra/modules/", KEY_END);
	keyAddBaseName (pluginInfo, plugin->name);
	KeySet * ksResult = ksNew (0, KS_END);
	plugin->kdbGet (plugin, ksResult, pluginInfo);

	Key * placementsKey = keyDup (pluginInfo);
	keyAddBaseName (placementsKey, "infos");
	keyAddBaseName (placementsKey, "placements");
	Key * placements = ksLookup (ksResult, placementsKey, 0);
	if (placements == NULL)
	{
		ELEKTRA_LOG_WARNING ("could not read placements from plugin");
		return 0;
	}
	char * placementList = strdup (keyString (placements));

	keyDel (pluginInfo);
	keyDel (placementsKey);
	ksDel (ksResult);

	return placementList;
}

static int listAddPlugin (Plugin * list, Plugin * plugin, char * placement)
{
	KeySet * newConfig = ksDup (list->config);

	// Find name for next item in plugins array
	Key * configBase = keyNew ("user/plugins", KEY_END);
	KeySet * array = elektraArrayGet (configBase, newConfig);
	Key * pluginItem = elektraArrayGetNextKey (array);
	ELEKTRA_NOT_NULL (pluginItem);
	keySetString (pluginItem, plugin->name);
	keyDel (configBase);

	// Create key with plugin handle
	Key * pluginHandle = keyDup (pluginItem);
	keyAddName (pluginHandle, "handle");
	keySetBinary (pluginHandle, &plugin, sizeof (plugin));

	// Create key with plugin placement
	char * placementType = placementToListPositionType (placement);
	if (placementType == NULL)
	{
		keyDel (configBase);
		keyDel (pluginItem);
		keyDel (pluginHandle);
		return 0;
	}
	Key * pluginPlacements = keyDup (pluginItem);
	keyAddName (pluginPlacements, "placements/");
	keyAddName (pluginPlacements, placementType);
	keySetString (pluginPlacements, placement);

	// Append keys to list plugin configuration
	ksAppendKey (newConfig, pluginItem);
	ksAppendKey (newConfig, pluginHandle);
	ksAppendKey (newConfig, pluginPlacements);

	ksDel (array);
	ksDel (list->config);

	// Apply new configuration
	list->config = newConfig;
	list->kdbOpen (list, NULL);

	return 1;
}

/**
 * @internal
 * Global mount given plugin at run-time.
 *
 * Reads placements from the plugin directly inserts the plugin.
 * Also supports adding itself to the list plugin at run-time if present
 * at requested global placement.
 *
 * @param  kdb    KDB handle
 * @param  plugin Plugin handle
 * @retval 0 on errors
 * @retval 1 on success
 */
static int mountGlobalPlugin (KDB * kdb, Plugin * plugin)
{
	char * placementList = getPluginPlacementList (plugin);

	// Parse plament list (contains placements from README.md seperated by whitespace)
	char * placement = strtok (placementList, " ");
	while (placement != NULL)
	{
		// Convert placement name to internal index
		int placementIndex = placementToPosition (placement);
		if (placementIndex == -1)
		{
			elektraFree (placementList);
			return 0;
		}

		if (kdb->globalPlugins[placementIndex][MAXONCE] == NULL)
		{
			// Insert directly as global plugin
			kdb->globalPlugins[placementIndex][MAXONCE] = plugin;
		}
		else
		{
			Plugin * pluginAtPlacement = kdb->globalPlugins[placementIndex][MAXONCE];
			// Add plugin to list plugin
			if (strcmp (pluginAtPlacement->name, "list") == 0)
			{
				ELEKTRA_LOG_DEBUG ("required position %s/maxonce already taken by list plugin, adding plugin", placement);
				int result = listAddPlugin (pluginAtPlacement, plugin, placement);
				if (!result)
				{
					ELEKTRA_LOG_WARNING ("could not add plugin to list plugin");
					elektraFree (placementList);
					return 0;
				}
			}
			else
			{
				// TODO manually add list module here, everything needed is stored in system/elektra/globalplugins
				ELEKTRA_LOG_WARNING ("required position postgetstorage/maxonce already taken by plugin %s, aborting!",
						     pluginAtPlacement->name);
			}
		}

		// Process next placement in list
		placement = strtok (NULL, " ");
	}

	elektraFree (placementList);

	return 1;
}

int elektraNotificationOpen (KDB * kdb, ElektraIoInterface * ioBinding)
{
	// Store I/O interface in kdb
	elektraSetIoBinding (kdb, ioBinding);

	Plugin * notificationPlugin = loadNotificationPlugin (kdb);
	if (!notificationPlugin)
	{
		return 0;
	}

	int mountResult = mountGlobalPlugin (kdb, notificationPlugin);
	if (!mountResult)
	{
		Key * errorKey = keyNew (0);
		elektraPluginClose (notificationPlugin, errorKey);
		keyDel (errorKey);
		return 0;
	}

	kdb->notificationPlugin = notificationPlugin;

	return 1;
}

int elektraNotificationClose (KDB * kdb)
{
	// Remove the I/O binding from kdb
	elektraSetIoBinding (kdb, NULL);

	// Unload the notification plugin
	Key * errorKey = keyNew (0);
	int result = elektraPluginClose (kdb->notificationPlugin, errorKey);
	int hasError = keyGetMeta (errorKey, "error") != NULL;
	if (!result || hasError)
	{
		ELEKTRA_LOG_WARNING ("elektraPluginClose failed result=%d", result);
		return 1;
	}
	else
	{
		return 0;
	}
}

static Plugin * getNotificationPlugin (KDB * kdb)
{
	if (kdb->notificationPlugin)
	{
		return kdb->notificationPlugin;
	}
	else
	{
		ELEKTRA_LOG_WARNING (
			"notificationPlugin not set. use elektraNotificationOpen before calling other elektraNotification-functions");
		return 0;
	}
}

int elektraNotificationRegisterInt (KDB * kdb, Key * key, int * variable)
{
	// Find notification plugin
	Plugin * notificationPlugin = getNotificationPlugin (kdb);
	if (!notificationPlugin)
	{
		return 0;
	}

	// Get register function from plugin
	const size_t * func = getPluginFunction (notificationPlugin, "registerInt");
	if (!func)
	{
		return 0;
	}

	// Call register function
	ElektraInternalnotificationRegisterInt registerFunc = (ElektraInternalnotificationRegisterInt) (*func);
	return registerFunc (notificationPlugin, key, variable);
}

int elektraNotificationRegisterCallback (KDB * kdb, Key * key, ElektraNotificationChangeCallback callback)
{
	// Find notification plugin
	Plugin * notificationPlugin = getNotificationPlugin (kdb);
	if (!notificationPlugin)
	{
		return 0;
	}

	// Get register function from plugin
	const size_t * func = getPluginFunction (notificationPlugin, "registerCallback");
	if (!func)
	{
		return 0;
	}

	// Call register function
	ElektraInternalnotificationRegisterCallback registerFunc = (ElektraInternalnotificationRegisterCallback) (*func);
	return registerFunc (notificationPlugin, key, callback);
}
