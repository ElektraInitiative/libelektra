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

typedef struct
{
	ElektraPluginProcess * pp;
	ElektraInvokeHandle * plugin;
	KeySet * pluginConfig;
} Process;
typedef int (*Elektra1Arg) (Key *);

static int elektraInvoke (ElektraInvokeHandle * handle, const char * elektraPluginFunctionName, Key * k)
{
	Elektra1Arg func = *(Elektra1Arg *)elektraInvokeGetFunction (handle, elektraPluginFunctionName);
	if (!func) return -2; // like elektraInvoke2Args handles it
	return func (k);
}

int elektraProcessOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp;

	if (process == NULL)
	{
		// process initialization
		process = elektraMalloc (sizeof (Process));
		KeySet * processConfig = elektraPluginGetConfig (handle);
		Key * pluginName = ksLookupByName (processConfig, "/plugin", KDB_O_NONE);

		KeySet * pluginConfig = ksDup (pluginConfig);
		ksLookup (processConfig, pluginName, KDB_O_DEL);

		process->pluginConfig = pluginConfig;
		process->plugin = elektraInvokeOpen (keyString (pluginName), process->pluginConfig);

		if (!process->plugin) return ELEKTRA_PLUGIN_STATUS_ERROR;

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

	int ret = elektraInvoke (process->plugin, "open", errorKey);
	// assume not found means this plugin doesn't export this function and do nothing
	// this logic is kept throughout the plugin
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	return ret;
}

int elektraProcessClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp = process->pp;
	if (elektraPluginProcessIsParent (pp))
	{
		int result = elektraPluginProcessSend (pp, ELEKTRA_PLUGIN_CLOSE, NULL, errorKey);
		if (elektraPluginProcessClose (pp))
		{
			elektraInvokeClose (process->plugin);
			ksDel (process->pluginConfig);
			elektraFree (process);
			elektraPluginSetData (handle, NULL);
		}
		return result;
	}

	int ret = elektraInvoke (process->plugin, "close", errorKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	return ret;
}

int elektraProcessGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/process"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/process", KEY_VALUE, "process plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/process/exports", KEY_END),
			       keyNew ("system/elektra/modules/process/exports/open", KEY_FUNC, elektraProcessOpen, KEY_END),
			       keyNew ("system/elektra/modules/process/exports/close", KEY_FUNC, elektraProcessClose, KEY_END),
			       keyNew ("system/elektra/modules/process/exports/get", KEY_FUNC, elektraProcessGet, KEY_END),
			       keyNew ("system/elektra/modules/process/exports/set", KEY_FUNC, elektraProcessSet, KEY_END),
			       keyNew ("system/elektra/modules/process/exports/error", KEY_FUNC, elektraProcessError, KEY_END),
			       keyNew ("system/elektra/modules/process/exports/checkconf", KEY_FUNC, elektraProcessCheckConfig, KEY_END),
#include ELEKTRA_README (process)
			       keyNew ("system/elektra/modules/process/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp = process->pp;
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGIN_GET, returned, parentKey);

	int ret = elektraInvoke2Args (process->plugin, "get", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	return ret;
}

int elektraProcessSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp = process->pp;
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGIN_SET, returned, parentKey);

	int ret = elektraInvoke2Args (process->plugin, "set", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	return ret;
}

int elektraProcessError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	Process * process = elektraPluginGetData (handle);
	ElektraPluginProcess * pp = process->pp;
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGIN_ERROR, returned, parentKey);

	int ret = elektraInvoke2Args (process->plugin, "error", returned, parentKey);
	if (ret == -2) return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	return ret;
}

int elektraProcessCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// We need the plugin key to know which plugin we should proxy
	Key * pluginNameKey = ksLookupByName (conf, "/plugin", KDB_O_NONE);
	if (pluginNameKey == NULL)
	{
		// TODO set error
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// and this key should obviously contain the plugin's name, so check for any name
	const char * pluginName = keyString (pluginNameKey);
	if (elektraStrCmp (pluginName, "(null)") == 0 || elektraStrCmp (pluginName, "(binary)") == 0)
	{
		// TODO set error
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
