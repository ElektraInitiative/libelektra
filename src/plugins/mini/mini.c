/**
 * @file
 *
 * @brief Source for mini plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "mini.h"

#include <kdbhelper.h>


int elektraMiniOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return 1; // success
}

int elektraMiniClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return 1; // success
}

int elektraMiniGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/mini"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/mini", KEY_VALUE, "mini plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/mini/exports", KEY_END),
			       keyNew ("system/elektra/modules/mini/exports/open", KEY_FUNC, elektraMiniOpen, KEY_END),
			       keyNew ("system/elektra/modules/mini/exports/close", KEY_FUNC, elektraMiniClose, KEY_END),
			       keyNew ("system/elektra/modules/mini/exports/get", KEY_FUNC, elektraMiniGet, KEY_END),
			       keyNew ("system/elektra/modules/mini/exports/set", KEY_FUNC, elektraMiniSet, KEY_END),
			       keyNew ("system/elektra/modules/mini/exports/error", KEY_FUNC, elektraMiniError, KEY_END),
			       keyNew ("system/elektra/modules/mini/exports/checkconf", KEY_FUNC, elektraMiniCheckConfig, KEY_END),
#include ELEKTRA_README (mini)
			       keyNew ("system/elektra/modules/mini/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys

	return 1; // success
}

int elektraMiniSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return 1; // success
}

int elektraMiniError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return 1; // success
}

int elektraMiniCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (mini)
{
	// clang-format off
	return elektraPluginExport ("mini",
		ELEKTRA_PLUGIN_OPEN,	&elektraMiniOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraMiniClose,
		ELEKTRA_PLUGIN_GET,	&elektraMiniGet,
		ELEKTRA_PLUGIN_SET,	&elektraMiniSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraMiniError,
		ELEKTRA_PLUGIN_END);
}
