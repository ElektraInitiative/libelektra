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


int elektraDiniOpen (Plugin * handle, Key * errorKey)
{
	Dini * dini = elektraMalloc (sizeof (Dini));
	dini->dump = elektraInvokeOpen ("dump", ksDup (elektraPluginGetConfig (handle)));
	dini->ini = elektraInvokeOpen ("ini", ksDup (elektraPluginGetConfig (handle)));

	dini->dumpErrors = keyNew ("", KEY_END);

	elektraPluginSetData (handle, dini);

	return dini->ini ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
}

int elektraDiniClose (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	Dini * dini = elektraPluginGetData (handle);

	elektraInvokeClose (dini->ini);
	elektraInvokeClose (dini->dump);

	keyDel (dini->dumpErrors);

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
#include ELEKTRA_README (dini)
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

	return elektraInvoke2Args (dini->ini, "get", returned, parentKey);
}

int elektraDiniSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	Dini * dini = elektraPluginGetData (handle);
	return elektraInvoke2Args (dini->ini, "set", returned, parentKey);
}

Plugin * ELEKTRA_PLUGIN_EXPORT (dini)
{
	// clang-format off
	return elektraPluginExport ("dini",
		ELEKTRA_PLUGIN_OPEN,	&elektraDiniOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDiniClose,
		ELEKTRA_PLUGIN_GET,	&elektraDiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraDiniSet,
		ELEKTRA_PLUGIN_END);
}
