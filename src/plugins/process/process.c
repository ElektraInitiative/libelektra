/**
 * @file
 *
 * @brief Source for process plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "process.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbinvoke.h>
#include <kdbpluginprocess.h>

#include <stdio.h>

typedef struct
{
	ElektraPluginProcess * pp;
	ElektraInvokeHandle * plugin;
	Key * pluginName;
	KeySet * pluginConfig;
} Process;

static int validPluginName (Key * pluginNameKey, Key * errorKey)
{
	if (pluginNameKey == NULL)
	{
		ELEKTRA_ADD_WARNING (198, errorKey, "Missing plugin configuration parameter plugin=<name of plugin to be proxied>");
		return 0;
	}

	// and this key should obviously contain the plugin's name, so check for any name
	// furthermore by invoking process in a process we'd create a deadloop, check that too
	const char * pluginName = keyString (pluginNameKey);
	if (elektraStrCmp (pluginName, "(null)") == 0 || elektraStrCmp (pluginName, "(binary)") == 0)
	{
		ELEKTRA_ADD_WARNINGF (198, errorKey, "Plugin configuration parameter plugin has an invalid value: %s", pluginName);
		return 0;
	}
	else if (elektraStrCmp (pluginName, "process") == 0)
	{
		ELEKTRA_ADD_WARNING (198, errorKey, "Cannot proxy the process plugin itself");
		return 0;
	}
	return 1;
}

static void cleanup (Plugin * handle, Process * process, Key * errorKey)
{
	if (process->plugin) elektraInvokeClose (process->plugin, errorKey);
	keyDel (process->pluginName);
	ksDel (process->pluginConfig);
	elektraFree (process);
	elektraPluginSetData (handle, NULL);
}

int elektraInvoke1Arg (ElektraInvokeHandle * handle, const char * elektraPluginFunctionName, Key * k)
{
	if (!handle || !elektraPluginFunctionName) return -2;

	// If we cast this right away although the function wasn't found it will cause a deadlock
	const void * rawFunc = elektraInvokeGetFunction (handle, elektraPluginFunctionName);

	if (!rawFunc) return -2;

	typedef int (*elektra1Arg) (Key *);
	elektra1Arg func = *(elektra1Arg *) rawFunc;
	return func (k);
}

int elektraProcessOpen (Plugin * handle, Key * errorKey)
{
	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp;

	if (process == NULL)
	{
		// process initialization
		process = elektraMalloc (sizeof (Process));
		KeySet * processConfig = elektraPluginGetConfig (handle);

		process->pluginName = ksLookupByName (processConfig, "/plugin", KDB_O_NONE);

		process->pluginConfig = ksDup (processConfig);
		ksLookup (process->pluginConfig, process->pluginName, KDB_O_DEL);

		process->plugin = NULL;

		// pluginprocess initialization
		if ((pp = elektraPluginProcessInit (errorKey)) == NULL) return ELEKTRA_PLUGIN_STATUS_ERROR;
		process->pp = pp;
		elektraPluginSetData (handle, process);
		if (!elektraPluginProcessIsParent (pp)) elektraPluginProcessStart (handle, pp);
	}
	else
	{
		pp = process->pp;
	}
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessOpen (pp, errorKey);

	// First time child initialization
	// elektraInvokeOpen will call the plugin's open function, this has to happen in the other process
	if (process->plugin == NULL && validPluginName (process->pluginName, errorKey))
	{
		process->plugin = elektraInvokeOpen (keyString (process->pluginName), process->pluginConfig, errorKey);
		if (!process->plugin)
		{
			ELEKTRA_SET_ERRORF (197, errorKey, "Failed to open the proxied plugin %s", keyString (process->pluginName));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		// The plugin will be only opened on the child side, so set the data again
		elektraPluginSetData (handle, process);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (process->plugin == NULL) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	// Otherwise just call the open function if it exists and don't reinitialize with elektraInvokeOpen
	int ret = elektraInvoke1Arg (process->plugin, "open", errorKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	return ret;
}

int elektraProcessClose (Plugin * handle, Key * errorKey)
{
	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp = process->pp;
	if (pp != NULL && elektraPluginProcessIsParent (pp))
	{
		ElektraPluginProcessCloseResult result = elektraPluginProcessClose (pp, errorKey);
		if (result.cleanedUp) cleanup (handle, process, errorKey);
		return result.result;
	}

	// Handle may be null if initialization failed in the child, ignore that it will exit anyway afterwards
	if (process->plugin == NULL) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke1Arg (process->plugin, "close", errorKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraProcessGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp = process->pp;

	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/process") && elektraPluginProcessIsParent (pp))
	{
		// Get the missing info about the plugin we proxy from the child process by mimicking its proxied plugin's module key
		Key * childKey = keyDup (parentKey);
		keySetBaseName (childKey, keyString (process->pluginName));
		KeySet * childReturned = ksDup (returned);
		int ret = elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_GET, childReturned, childKey);

		if (ret != ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			KeySet * contract = ksNew (
				30, keyNew ("system/elektra/modules/process", KEY_VALUE, "process plugin waits for your orders", KEY_END),
				keyNew ("system/elektra/modules/process/exports", KEY_END),
				keyNew ("system/elektra/modules/process/exports/open", KEY_FUNC, elektraProcessOpen, KEY_END),
				keyNew ("system/elektra/modules/process/exports/close", KEY_FUNC, elektraProcessClose, KEY_END),
				keyNew ("system/elektra/modules/process/exports/get", KEY_FUNC, elektraProcessGet, KEY_END),
				keyNew ("system/elektra/modules/process/exports/set", KEY_FUNC, elektraProcessSet, KEY_END),
				keyNew ("system/elektra/modules/process/exports/error", KEY_FUNC, elektraProcessError, KEY_END),
				keyNew ("system/elektra/modules/process/exports/checkconf", KEY_FUNC, elektraProcessCheckConfig, KEY_END),
#include ELEKTRA_README (process)
				keyNew ("system/elektra/modules/process/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);

			// Now adjust the placements to whatever our proxied plugin says
			Key * placementsKey = ksLookupByName (contract, "system/elektra/modules/process/infos/placements", KDB_O_NONE);
			keyAddBaseName (childKey, "infos");
			keyAddBaseName (childKey, "placements");

			Key * childPlacementsKey = ksLookup (childReturned, childKey, KDB_O_NONE);
			keySetString (placementsKey, keyString (childPlacementsKey));

			ksAppend (returned, contract);
			ksDel (contract);
		}

		keyDel (childKey);
		ksDel (childReturned);
		return ret;
	}

	// Otherwise business as usual
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_GET, returned, parentKey);

	if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke2Args (process->plugin, "get", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	return ret;
}

int elektraProcessSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp = process->pp;
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_SET, returned, parentKey);

	if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke2Args (process->plugin, "set", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	return ret;
}

int elektraProcessError (Plugin * handle, KeySet * returned, Key * parentKey)
{
	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp = process->pp;
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_ERROR, returned, parentKey);

	if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke2Args (process->plugin, "error", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	return ret;
}

int elektraProcessCheckConfig (Key * errorKey, KeySet * conf)
{
	// We need the plugin key to know which plugin we should proxy
	Key * pluginNameKey = ksLookupByName (conf, "/plugin", KDB_O_NONE);
	if (!validPluginName (pluginNameKey, errorKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (process)
{
	// clang-format off
	return elektraPluginExport ("process",
		ELEKTRA_PLUGIN_OPEN,	&elektraProcessOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraProcessClose,
		ELEKTRA_PLUGIN_GET,		&elektraProcessGet,
		ELEKTRA_PLUGIN_SET,		&elektraProcessSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraProcessError,
		ELEKTRA_PLUGIN_END);
}
