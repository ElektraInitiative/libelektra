/**
 * @file
 *
 * @brief Source for reference plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "reference.h"

#include <kdbhelper.h>


int elektraReferenceOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraReferenceClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraReferenceGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/reference"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/reference", KEY_VALUE, "reference plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/reference/exports", KEY_END),
			       keyNew ("system/elektra/modules/reference/exports/open", KEY_FUNC, elektraReferenceOpen, KEY_END),
			       keyNew ("system/elektra/modules/reference/exports/close", KEY_FUNC, elektraReferenceClose, KEY_END),
			       keyNew ("system/elektra/modules/reference/exports/get", KEY_FUNC, elektraReferenceGet, KEY_END),
			       keyNew ("system/elektra/modules/reference/exports/set", KEY_FUNC, elektraReferenceSet, KEY_END),
			       keyNew ("system/elektra/modules/reference/exports/error", KEY_FUNC, elektraReferenceError, KEY_END),
			       keyNew ("system/elektra/modules/reference/exports/checkconf", KEY_FUNC, elektraReferenceCheckConfig, KEY_END),
#include ELEKTRA_README (reference)
			       keyNew ("system/elektra/modules/reference/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraReferenceSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraReferenceError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraReferenceCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (reference)
{
	// clang-format off
	return elektraPluginExport ("reference",
		ELEKTRA_PLUGIN_OPEN,	&elektraReferenceOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraReferenceClose,
		ELEKTRA_PLUGIN_GET,	&elektraReferenceGet,
		ELEKTRA_PLUGIN_SET,	&elektraReferenceSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraReferenceError,
		ELEKTRA_PLUGIN_END);
}
