/**
 * @file
 *
 * @brief Source for dini plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "dini.h"

#include <kdbhelper.h>


int elektraDiniOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	Dini * dini = elektraMalloc (sizeof (Dini));

	dini->dumpConfig = ksDup (elektraPluginGetConfig (handle));
	dini->iniConfig = ksDup (elektraPluginGetConfig (handle));
	dini->dump = elektraInvokeOpen ("dump", dini->dumpConfig, 0);
	dini->ini = elektraInvokeOpen ("ini", dini->iniConfig, 0);
	dini->bin = elektraInvokeOpen ("binary", dini->iniConfig, 0);

	dini->dumpErrors = keyNew ("", KEY_END);

	elektraPluginSetData (handle, dini);

	return dini->ini ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
}

int elektraDiniClose (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	Dini * dini = elektraPluginGetData (handle);

	elektraInvokeClose (dini->bin, 0);
	elektraInvokeClose (dini->ini, 0);
	elektraInvokeClose (dini->dump, 0);

	keyDel (dini->dumpErrors);
	ksDel (dini->dumpConfig);
	ksDel (dini->iniConfig);

	elektraFree (dini);
	elektraPluginSetData (handle, NULL);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraDiniGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/dini"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/dini", KEY_VALUE, "dini plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/dini/exports", KEY_END),
			       keyNew ("system/elektra/modules/dini/exports/open", KEY_FUNC, elektraDiniOpen, KEY_END),
			       keyNew ("system/elektra/modules/dini/exports/close", KEY_FUNC, elektraDiniClose, KEY_END),
			       keyNew ("system/elektra/modules/dini/exports/get", KEY_FUNC, elektraDiniGet, KEY_END),
			       keyNew ("system/elektra/modules/dini/exports/set", KEY_FUNC, elektraDiniSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/dini/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	// get all keys
	Dini * dini = elektraPluginGetData (handle);
	keySetName (dini->dumpErrors, keyName (parentKey));
	keySetString (dini->dumpErrors, keyString (parentKey));
	if (elektraInvoke2Args (dini->dump, "get", returned, dini->dumpErrors) != -1)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	int ret = elektraInvoke2Args (dini->ini, "get", returned, parentKey);
	if (dini->bin)
	{
		ret |= elektraInvoke2Args (dini->bin, "get", returned, parentKey); // postgetstorage
	}
	return ret;
}

int elektraDiniSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	Dini * dini = elektraPluginGetData (handle);

	int ret = 0;
	if (dini->bin)
	{
		elektraInvoke2Args (dini->bin, "set", returned, parentKey); // presetstorage
	}
	ret |= elektraInvoke2Args (dini->ini, "set", returned, parentKey);
	return ret;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("dini",
		ELEKTRA_PLUGIN_OPEN,	&elektraDiniOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDiniClose,
		ELEKTRA_PLUGIN_GET,	&elektraDiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraDiniSet,
		ELEKTRA_PLUGIN_END);
}
