/**
 * @file
 *
 * @brief Source for xfconf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "xfconf.h"

#include <kdbhelper.h>
#include <xfconf/xfconf.h>

int elektraXfconfOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional
	xfconf_init(NULL);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraXfconfClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraXfconfGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/xfconf"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/xfconf", KEY_VALUE, "xfconf plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports", KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/open", KEY_FUNC, elektraXfconfOpen, KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/close", KEY_FUNC, elektraXfconfClose, KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/get", KEY_FUNC, elektraXfconfGet, KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/set", KEY_FUNC, elektraXfconfSet, KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/commit", KEY_FUNC, elektraXfconfCommit, KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/error", KEY_FUNC, elektraXfconfError, KEY_END),
			       keyNew ("system:/elektra/modules/xfconf/exports/checkconf", KEY_FUNC, elektraXfconfCheckConf, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/xfconf/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraXfconfSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraXfconfError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraXfconfCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// commit changes
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraXfconfCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("xfconf",
		ELEKTRA_PLUGIN_OPEN,	&elektraXfconfOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraXfconfClose,
		ELEKTRA_PLUGIN_GET,	&elektraXfconfGet,
		ELEKTRA_PLUGIN_SET,	&elektraXfconfSet,
		ELEKTRA_PLUGIN_COMMIT,  &elektraXfconfCommit,
		ELEKTRA_PLUGIN_ERROR,	&elektraXfconfError,
		ELEKTRA_PLUGIN_END);
}
