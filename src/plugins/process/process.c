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
	ElektraKey * pluginName;
	ElektraKeyset * pluginConfig;
} Process;

static int validPluginName (ElektraKey * pluginNameKey, ElektraKey * errorKey)
{
	if (pluginNameKey == NULL)
	{
		ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Missing plugin configuration parameter plugin=<name of plugin to be proxied>");
		return 0;
	}

	// and this key should obviously contain the plugin's name, so check for any name
	// furthermore by invoking process in a process we'd create a deadloop, check that too
	const char * pluginName = elektraKeyString (pluginNameKey);
	if (elektraStrCmp (pluginName, "(null)") == 0 || elektraKeyIsBinary (pluginNameKey))
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

static void cleanup (Process * process, ElektraKey * errorKey)
{
	if (process->plugin) elektraInvokeClose (process->plugin, errorKey);
	if (process->pluginName) elektraKeyDel (process->pluginName);
	elektraKeysetDel (process->pluginConfig);
	elektraFree (process);
}

int elektraInvoke1Arg (ElektraInvokeHandle * handle, const char * elektraPluginFunctionName, ElektraKey * k)
{
	if (!handle || !elektraPluginFunctionName) return -2;

	// If we cast this right away although the function wasn't found it will cause a deadlock
	const void * rawFunc = elektraInvokeGetFunction (handle, elektraPluginFunctionName);

	if (!rawFunc) return -2;

	typedef int (*elektra1Arg) (ElektraKey *);
	elektra1Arg func = *(elektra1Arg *) rawFunc;
	return func (k);
}

static int isContractKey (ElektraKey * key)
{
	return !elektraStrCmp (elektraKeyName (key), "system:/elektra/modules/process");
}

int elektraProcessOpen (Plugin * handle, ElektraKey * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp == NULL)
	{
		// process initialization
		Process * process = elektraMalloc (sizeof (Process));
		ElektraKeyset * processConfig = elektraPluginGetConfig (handle);
		process->pluginName = elektraKeysetLookupByName (processConfig, "/plugin", ELEKTRA_KDB_O_POP);
		process->pluginConfig = elektraKeysetDup (processConfig);
		elektraKeysetAppendKey (processConfig, process->pluginName);
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
		process->plugin = elektraInvokeOpen (elektraKeyString (process->pluginName), process->pluginConfig, errorKey);
		if (!process->plugin)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Failed to open the proxied plugin %s", elektraKeyString (process->pluginName));
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

int elektraProcessClose (Plugin * handle, ElektraKey * errorKey)
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

static void adjustContract (ElektraKeyset * pluginContract, ElektraKeyset * contract)
{
	elektraKeysetRewind (pluginContract);
	ElektraKey * cur;
	while ((cur = elektraKeysetNext (pluginContract)) != NULL)
	{
		ElektraKey * cpy = elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL);
		elektraKeySetBaseName (cpy, NULL);
		if (!elektraStrCmp ("infos", elektraKeyBaseName (cpy)))
		{
			elektraKeySetBaseName (cpy, NULL);
			elektraKeySetBaseName (cpy, NULL);
			elektraKeyAddBaseName (cpy, "process");
			elektraKeyAddBaseName (cpy, "infos");
			elektraKeyAddBaseName (cpy, elektraKeyBaseName (cur));
			ElektraKey * infoKey = elektraKeysetLookup (contract, cpy, ELEKTRA_KDB_O_NONE);
			elektraKeySetString (infoKey, elektraKeyString (cpy));
		}
		elektraKeyDel (cpy);
	}
}


int elektraProcessGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	Process * process = elektraPluginProcessGetData (pp);

	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_GET, returned, parentKey);

	if (isContractKey (parentKey))
	{
		ElektraKeyset * processConfig = elektraPluginGetConfig (handle);
		ElektraKey * pluginName = elektraKeysetLookupByName (processConfig, "/plugin", ELEKTRA_KDB_O_NONE);

		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/process", ELEKTRA_KEY_VALUE, "process plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/process/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/process/exports/open", ELEKTRA_KEY_FUNC, elektraProcessOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/process/exports/close", ELEKTRA_KEY_FUNC, elektraProcessClose, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/process/exports/get", ELEKTRA_KEY_FUNC, elektraProcessGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/process/exports/set", ELEKTRA_KEY_FUNC, elektraProcessSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/process/exports/error", ELEKTRA_KEY_FUNC, elektraProcessError, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/process/exports/checkconf", ELEKTRA_KEY_FUNC, elektraProcessCheckConf, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/process/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		if (!validPluginName (pluginName, parentKey) || !process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

		ElektraKey * pluginParentKey = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
		elektraKeySetBaseName (pluginParentKey, elektraKeyString (pluginName));

		ElektraKeyset * pluginContract = elektraKeysetNew (30, ELEKTRA_KS_END);
		elektraInvoke2Args (process->plugin, "get", pluginContract, pluginParentKey);
		elektraKeyDel (pluginParentKey);
		if (elektraKeysetGetSize (pluginContract) == 0)
		{
			ELEKTRA_SET_INTERFACE_ERRORF (parentKey, "Failed to get the contract for %s", elektraKeyString (pluginName));
			elektraKeysetDel (pluginContract);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		adjustContract (pluginContract, returned);
		elektraKeysetDel (pluginContract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke2Args (process->plugin, "get", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	return ret;
}

int elektraProcessSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	Process * process = elektraPluginProcessGetData (pp);

	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_SET, returned, parentKey);

	if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke2Args (process->plugin, "set", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	return ret;
}

int elektraProcessError (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	Process * process = elektraPluginProcessGetData (pp);

	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_ERROR, returned, parentKey);

	if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	int ret = elektraInvoke2Args (process->plugin, "error", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	return ret;
}

int elektraProcessCheckConf (ElektraKey * errorKey, ElektraKeyset * conf)
{
	// We need the plugin key to know which plugin we should proxy
	ElektraKey * pluginNameKey = elektraKeysetLookupByName (conf, "/plugin", ELEKTRA_KDB_O_NONE);
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
