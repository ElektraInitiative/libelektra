/**
 * @file
 *
 * @brief Source for template plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "template.h"

#include <kdbhelper.h>


int elektraTemplateOpen (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTemplateClose (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTemplateGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/template"))
	{
		ElektraKeyset * contract =
			ksNew (30, keyNew ("system:/elektra/modules/template", ELEKTRA_KEY_VALUE, "template plugin waits for your orders", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/template/exports", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/open", ELEKTRA_KEY_FUNC, elektraTemplateOpen, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/close", ELEKTRA_KEY_FUNC, elektraTemplateClose, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/get", ELEKTRA_KEY_FUNC, elektraTemplateGet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/set", ELEKTRA_KEY_FUNC, elektraTemplateSet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/commit", ELEKTRA_KEY_FUNC, elektraTemplateCommit, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/error", ELEKTRA_KEY_FUNC, elektraTemplateError, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/template/exports/checkconf", ELEKTRA_KEY_FUNC, elektraTemplateCheckConf, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/template/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraTemplateSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraTemplateError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTemplateCommit (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// commit changes
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraTemplateCheckConf (ElektraKey * errorKey ELEKTRA_UNUSED, ElektraKeyset * conf ELEKTRA_UNUSED)
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
