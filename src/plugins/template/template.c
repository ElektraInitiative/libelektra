/**
 * @file
 *
 * @brief Source for template plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "template.h"

#include <elektra/kdbhelper.h>


int elektraTemplateOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTemplateClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTemplateGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/template"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/template", KEY_VALUE, "template plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/template/exports", KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/open", KEY_FUNC, elektraTemplateOpen, KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/close", KEY_FUNC, elektraTemplateClose, KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/get", KEY_FUNC, elektraTemplateGet, KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/set", KEY_FUNC, elektraTemplateSet, KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/commit", KEY_FUNC, elektraTemplateCommit, KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/error", KEY_FUNC, elektraTemplateError, KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/checkconf", KEY_FUNC, elektraTemplateCheckConf, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/template/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraTemplateSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraTemplateError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTemplateCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// commit changes
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTemplateCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("template",
		ELEKTRA_PLUGIN_OPEN,	&elektraTemplateOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTemplateClose,
		ELEKTRA_PLUGIN_GET,	&elektraTemplateGet,
		ELEKTRA_PLUGIN_SET,	&elektraTemplateSet,
		ELEKTRA_PLUGIN_COMMIT,  &elektraTemplateCommit,
		ELEKTRA_PLUGIN_ERROR,	&elektraTemplateError,
		ELEKTRA_PLUGIN_END);
}
