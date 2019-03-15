/**
 * @file
 *
 * @brief Source for hexcolor plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "hexcolor.h"

#include <kdbhelper.h>


int elektraHexcolorOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraHexcolorClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraHexcolorGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/hexcolor"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/hexcolor", KEY_VALUE, "hexcolor plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports", KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports/open", KEY_FUNC, elektraHexcolorOpen, KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports/close", KEY_FUNC, elektraHexcolorClose, KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports/get", KEY_FUNC, elektraHexcolorGet, KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports/set", KEY_FUNC, elektraHexcolorSet, KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports/error", KEY_FUNC, elektraHexcolorError, KEY_END),
			       keyNew ("system/elektra/modules/hexcolor/exports/checkconf", KEY_FUNC, elektraHexcolorCheckConfig, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/hexcolor/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraHexcolorSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraHexcolorError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraHexcolorCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("hexcolor",
		ELEKTRA_PLUGIN_OPEN,	&elektraHexcolorOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraHexcolorClose,
		ELEKTRA_PLUGIN_GET,	&elektraHexcolorGet,
		ELEKTRA_PLUGIN_SET,	&elektraHexcolorSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraHexcolorError,
		ELEKTRA_PLUGIN_END);
}
