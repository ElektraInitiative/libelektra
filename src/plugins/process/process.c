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
	ElektraInvokeHandle * plugin;
	Key * pluginName;
	KeySet * pluginConfig;
} Process;

static int validPluginName (Key * pluginNameKey, Key * errorKey)
{
	if (pluginNameKey == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Missing plugin configuration parameter plugin=<name of plugin to be proxied>");
		return 0;
	}

	// and this key should obviously contain the plugin's name, so check for any name
	// furthermore by invoking process in a process we'd create a deadloop, check that too
	const char * pluginName = keyString (pluginNameKey);
	if (elektraStrCmp (pluginName, "(null)") == 0 || keyIsBinary (pluginNameKey))
	{
		ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (errorKey, "Plugin configuration parameter plugin has an invalid value: %s",
							  pluginName);
		return 0;
	}
	else if (elektraStrCmp (pluginName, "process") == 0)
	{
		ELEKTRA_ADD_INTERFACE_WARNING (errorKey, "Cannot proxy the process plugin itself");
		return 0;
	}
	return 1;
}

static void cleanup (Process * process, Key * errorKey)
{
	if (process->plugin) elektraInvokeClose (process->plugin, errorKey);
	if (process->pluginName) keyDel (process->pluginName);
	ksDel (process->pluginConfig);
	elektraFree (process);
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

static int isContractKey (Key * key)
{
	return !elektraStrCmp (keyName (key), "system:/elektra/modules/process");
}

int elektraProcessOpen (Plugin * handle, Key * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp == NULL)
	{
		// process initialization
		Process * process = elektraMalloc (sizeof (Process));
		KeySet * processConfig = elektraPluginGetConfig (handle);
		process->pluginName = ksLookupByName (processConfig, "/plugin", KDB_O_POP);
		process->pluginConfig = ksDup (processConfig);
		ksAppendKey (processConfig, process->pluginName);
		process->plugin = NULL;

		if ((pp = elektraPluginProcessInit (errorKey)) == NULL) return ELEKTRA_PLUGIN_STATUS_ERROR;

		elektraPluginProcessSetData (pp, process);
		elektraPluginSetData (handle, pp);
		if (!elektraPluginProcessIsParent (pp)) elektraPluginProcessStart (handle, pp);
	}

	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessOpen (pp, errorKey);

	Process * process = elektraPluginProcessGetData (pp);

	// First time child initialization
	// elektraInvokeOpen will call the plugin's open function, this has to happen in the other process

	if (process->plugin == NULL && !isContractKey (errorKey) && validPluginName (process->pluginName, errorKey))
	{
		process->plugin = elektraInvokeOpen (keyString (process->pluginName), process->pluginConfig, errorKey);
		if (!process->plugin)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Failed to open the proxied plugin %s", keyString (process->pluginName));
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
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
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (!pp) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	Process * process = elektraPluginProcessGetData (pp);
	if (elektraPluginProcessIsParent (pp))
	{
		ElektraPluginProcessCloseResult result = elektraPluginProcessClose (pp, errorKey);
		if (result.cleanedUp)
		{
			cleanup (process, errorKey);
			elektraPluginSetData (handle, NULL);
		}
		return result.result;
	}


	// Handle may be null if initialization failed in the child, ignore that it will exit anyway afterwards
	if (process->plugin == NULL) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke1Arg (process->plugin, "close", errorKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static void adjustContract (KeySet * pluginContract, KeySet * contract)
{
	ksRewind (pluginContract);
	Key * cur;
	while ((cur = ksNext (pluginContract)) != NULL)
	{
		Key * cpy = keyDup (cur, KEY_CP_ALL);
		keySetBaseName (cpy, NULL);
		if (!elektraStrCmp ("infos", keyBaseName (cpy)))
		{
			keySetBaseName (cpy, NULL);
			keySetBaseName (cpy, NULL);
			keyAddBaseName (cpy, "process");
			keyAddBaseName (cpy, "infos");
			keyAddBaseName (cpy, keyBaseName (cur));
			Key * infoKey = ksLookup (contract, cpy, KDB_O_NONE);
			keySetString (infoKey, keyString (cpy));
		}
		keyDel (cpy);
	}
}


int elektraProcessGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	Process * process = elektraPluginProcessGetData (pp);

	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_GET, returned, parentKey);

	if (isContractKey (parentKey))
	{
		KeySet * processConfig = elektraPluginGetConfig (handle);
		Key * pluginName = ksLookupByName (processConfig, "/plugin", KDB_O_NONE);

		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/process", KEY_VALUE, "process plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/process/exports", KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/open", KEY_FUNC, elektraProcessOpen, KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/close", KEY_FUNC, elektraProcessClose, KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/get", KEY_FUNC, elektraProcessGet, KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/set", KEY_FUNC, elektraProcessSet, KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/error", KEY_FUNC, elektraProcessError, KEY_END),
			       keyNew ("system:/elektra/modules/process/exports/checkconf", KEY_FUNC, elektraProcessCheckConf, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/process/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		if (!validPluginName (pluginName, parentKey) || !process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

		Key * pluginParentKey = keyDup (parentKey, KEY_CP_ALL);
		keySetBaseName (pluginParentKey, keyString (pluginName));

		KeySet * pluginContract = ksNew (30, KS_END);
		elektraInvoke2Args (process->plugin, "get", pluginContract, pluginParentKey);
		keyDel (pluginParentKey);
		if (ksGetSize (pluginContract) == 0)
		{
			ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "Failed to get the contract for %s", keyString (pluginName));
			ksDel (pluginContract);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		adjustContract (pluginContract, returned);
		ksDel (pluginContract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke2Args (process->plugin, "get", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	return ret;
}

int elektraProcessSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	Process * process = elektraPluginProcessGetData (pp);

	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_SET, returned, parentKey);

	if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke2Args (process->plugin, "set", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	return ret;
}

int elektraProcessError (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	Process * process = elektraPluginProcessGetData (pp);

	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_ERROR, returned, parentKey);

	if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke2Args (process->plugin, "error", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	return ret;
}

int elektraProcessCheckConf (Key * errorKey, KeySet * conf)
{
	// We need the plugin key to know which plugin we should proxy
	Key * pluginNameKey = ksLookupByName (conf, "/plugin", KDB_O_NONE);
	if (!validPluginName (pluginNameKey, errorKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("process",
		ELEKTRA_PLUGIN_OPEN,	&elektraProcessOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraProcessClose,
		ELEKTRA_PLUGIN_GET,	&elektraProcessGet,
		ELEKTRA_PLUGIN_SET,		&elektraProcessSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraProcessError,
		ELEKTRA_PLUGIN_END);
}
