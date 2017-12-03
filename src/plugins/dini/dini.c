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

#include <kdbmodule.h>
#include <kdbprivate.h> // for elektraPluginOpen/Close+plugin structure


int elektraDiniOpen (Plugin * handle, Key * errorKey)
{
	Dini * dini = elektraMalloc (sizeof (Dini));
	dini->modules = ksNew (24, KS_END);
	elektraModulesInit (dini->modules, errorKey);
	dini->dump = elektraPluginOpen("dump", dini->modules, ksDup (elektraPluginGetConfig (handle)), 0); // ignore errors
	dini->ini = elektraPluginOpen("ini", dini->modules, ksDup (elektraPluginGetConfig (handle)), errorKey);

	dini->dumpErrors = keyNew ("", KEY_END);

	elektraPluginSetData (handle, dini);

	return dini->ini ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
}

int elektraDiniClose (Plugin * handle, Key * errorKey)
{
	Dini * dini = elektraPluginGetData (handle);

	elektraPluginClose (dini->ini, errorKey);
	elektraPluginClose (dini->dump, errorKey);

	elektraModulesClose (dini->modules, errorKey);

	keyDel (dini->dumpErrors);

	elektraFree (dini);
	elektraPluginSetData (handle, NULL);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraDiniGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
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
			       keyNew ("system/elektra/modules/dini/exports/error", KEY_FUNC, elektraDiniError, KEY_END),
			       keyNew ("system/elektra/modules/dini/exports/checkconf", KEY_FUNC, elektraDiniCheckConfig, KEY_END),
#include ELEKTRA_README (dini)
			       keyNew ("system/elektra/modules/dini/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	// get all keys
	Dini * dini = elektraPluginGetData (handle);
	if (dini->dump && dini->dump->kdbGet (handle, returned, dini->dumpErrors) != -1)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return dini->ini->kdbGet (handle, returned, parentKey);
}

int elektraDiniSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraDiniError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraDiniCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (dini)
{
	// clang-format off
	return elektraPluginExport ("dini",
		ELEKTRA_PLUGIN_OPEN,	&elektraDiniOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraDiniClose,
		ELEKTRA_PLUGIN_GET,	&elektraDiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraDiniSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraDiniError,
		ELEKTRA_PLUGIN_END);
}
