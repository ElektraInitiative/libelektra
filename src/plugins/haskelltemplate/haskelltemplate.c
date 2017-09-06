/**
 * @file
 *
 * @brief Source for haskelltemplate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "haskelltemplate.h"
#include "Haskelltemplate_stub.h"

#include <kdbhelper.h>

#include <stdio.h>

int elektraHaskelltemplateOpen (Plugin * handle, Key * errorKey)
{
	static char *argv[] = { "haskelltemplate", 0 }, **argvPtr = argv;
	static int argc = 1;
	// Startup the haskell runtime with some dummy args
	hs_init (&argc, &argvPtr);
	return hs_elektraHaskelltemplateOpen (handle, errorKey);
}

int elektraHaskelltemplateClose (Plugin * handle, Key * errorKey)
{
	int ret = hs_elektraHaskelltemplateClose (handle, errorKey);
	// Shutdown the haskell runtime
	hs_exit ();
	return ret;
}

int elektraHaskelltemplateGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/haskelltemplate"))
	{
		KeySet * contract = ksNew (
			30,
			keyNew ("system/elektra/modules/haskelltemplate", KEY_VALUE, "haskelltemplate plugin waits for your orders",
				KEY_END),
			keyNew ("system/elektra/modules/haskelltemplate/exports", KEY_END),
			keyNew ("system/elektra/modules/haskelltemplate/exports/open", KEY_FUNC, elektraHaskelltemplateOpen, KEY_END),
			keyNew ("system/elektra/modules/haskelltemplate/exports/close", KEY_FUNC, elektraHaskelltemplateClose, KEY_END),
			keyNew ("system/elektra/modules/haskelltemplate/exports/get", KEY_FUNC, elektraHaskelltemplateGet, KEY_END),
			keyNew ("system/elektra/modules/haskelltemplate/exports/set", KEY_FUNC, elektraHaskelltemplateSet, KEY_END),
			keyNew ("system/elektra/modules/haskelltemplate/exports/error", KEY_FUNC, elektraHaskelltemplateError, KEY_END),
			keyNew ("system/elektra/modules/haskelltemplate/exports/checkconf", KEY_FUNC, elektraHaskelltemplateCheckConfig,
				KEY_END),
#include ELEKTRA_README (haskelltemplate)
			keyNew ("system/elektra/modules/haskelltemplate/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	return hs_elektraHaskelltemplateGet (handle, returned, parentKey);
}

int elektraHaskelltemplateSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	// set all keys
	// this function is optional
	return hs_elektraHaskelltemplateSet (handle, returned, parentKey);
}

int elektraHaskelltemplateError (Plugin * handle, KeySet * returned, Key * parentKey)
{
	// handle errors (commit failed)
	// this function is optional
	return hs_elektraHaskelltemplateError (handle, returned, parentKey);
}

int elektraHaskelltemplateCheckConfig (Key * errorKey, KeySet * conf)
{
	// validate plugin configuration
	// this function is optional
	return hs_elektraHaskelltemplateCheckConfig (errorKey, conf);
}

Plugin * ELEKTRA_PLUGIN_EXPORT (haskelltemplate)
{
	// clang-format off
	return elektraPluginExport ("haskelltemplate",
		ELEKTRA_PLUGIN_OPEN,	&elektraHaskelltemplateOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraHaskelltemplateClose,
		ELEKTRA_PLUGIN_GET,	&elektraHaskelltemplateGet,
		ELEKTRA_PLUGIN_SET,	&elektraHaskelltemplateSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraHaskelltemplateError,
		ELEKTRA_PLUGIN_END);
}
