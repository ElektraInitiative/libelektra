/**
 * @file
 *
 * @brief Source for iterate plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "iterate.h"

#include <kdbhelper.h>


int elektraIterateOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic

	return 1; // success
}

int elektraIterateClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down

	return 1; // success
}

int elektraIterateGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/iterate"))
	{
		KeySet * contract = ksNew (30,
		keyNew ("system/elektra/modules/iterate",
			KEY_VALUE, "iterate plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/iterate/exports", KEY_END),
		keyNew ("system/elektra/modules/iterate/exports/open",
			KEY_FUNC, elektraIterateOpen, KEY_END),
		keyNew ("system/elektra/modules/iterate/exports/close",
			KEY_FUNC, elektraIterateClose, KEY_END),
		keyNew ("system/elektra/modules/iterate/exports/get",
			KEY_FUNC, elektraIterateGet, KEY_END),
		keyNew ("system/elektra/modules/iterate/exports/set",
			KEY_FUNC, elektraIterateSet, KEY_END),
		keyNew ("system/elektra/modules/iterate/exports/error",
			KEY_FUNC, elektraIterateError, KEY_END),
#include ELEKTRA_README (iterate)
		keyNew ("system/elektra/modules/iterate/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys

	return 1; // success
}

int elektraIterateSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys

	return 1; // success
}

int elektraIterateError (Plugin *handle ELEKTRA_UNUSED, KeySet *returned ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	// set all keys

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (iterate)
{
	return elektraPluginExport ("iterate",
		ELEKTRA_PLUGIN_OPEN,	&elektraIterateOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraIterateClose,
		ELEKTRA_PLUGIN_GET,	&elektraIterateGet,
		ELEKTRA_PLUGIN_SET,	&elektraIterateSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraIterateError,
		ELEKTRA_PLUGIN_END);
}

