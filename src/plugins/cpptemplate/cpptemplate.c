/**
 * @file
 *
 * @brief Source for cpptemplate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "cpptemplate.h"

#include <kdbhelper.h>


int elektraCpptemplateOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCpptemplateClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCpptemplateGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/cpptemplate"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/cpptemplate", KEY_VALUE, "cpptemplate plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/cpptemplate/exports", KEY_END),
			keyNew ("system/elektra/modules/cpptemplate/exports/open", KEY_FUNC, elektraCpptemplateOpen, KEY_END),
			keyNew ("system/elektra/modules/cpptemplate/exports/close", KEY_FUNC, elektraCpptemplateClose, KEY_END),
			keyNew ("system/elektra/modules/cpptemplate/exports/get", KEY_FUNC, elektraCpptemplateGet, KEY_END),
			keyNew ("system/elektra/modules/cpptemplate/exports/set", KEY_FUNC, elektraCpptemplateSet, KEY_END),
			keyNew ("system/elektra/modules/cpptemplate/exports/error", KEY_FUNC, elektraCpptemplateError, KEY_END),
			keyNew ("system/elektra/modules/cpptemplate/exports/checkconf", KEY_FUNC, elektraCpptemplateCheckConfig, KEY_END),
#include ELEKTRA_README (cpptemplate)
			keyNew ("system/elektra/modules/cpptemplate/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraCpptemplateSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraCpptemplateError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCpptemplateCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (cpptemplate)
{
	// clang-format off
	return elektraPluginExport ("cpptemplate",
		ELEKTRA_PLUGIN_OPEN,	&elektraCpptemplateOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCpptemplateClose,
		ELEKTRA_PLUGIN_GET,	&elektraCpptemplateGet,
		ELEKTRA_PLUGIN_SET,	&elektraCpptemplateSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraCpptemplateError,
		ELEKTRA_PLUGIN_END);
}
