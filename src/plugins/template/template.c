/**
 * @file
 *
 * @brief Source for template plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "template.h"

#include <kdbhelper.h>


int elektraTemplateOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic

	return 1; // success
}

int elektraTemplateClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down

	return 1; // success
}

int elektraTemplateGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/template"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/template", KEY_VALUE, "template plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/template/exports", KEY_END),
			       keyNew ("system/elektra/modules/template/exports/open", KEY_FUNC, elektraTemplateOpen, KEY_END),
			       keyNew ("system/elektra/modules/template/exports/close", KEY_FUNC, elektraTemplateClose, KEY_END),
			       keyNew ("system/elektra/modules/template/exports/get", KEY_FUNC, elektraTemplateGet, KEY_END),
			       keyNew ("system/elektra/modules/template/exports/set", KEY_FUNC, elektraTemplateSet, KEY_END),
			       keyNew ("system/elektra/modules/template/exports/error", KEY_FUNC, elektraTemplateError, KEY_END),
#include ELEKTRA_README (template)
			       keyNew ("system/elektra/modules/template/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys

	return 1; // success
}

int elektraTemplateSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys

	return 1; // success
}

int elektraTemplateError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (template)
{
	// clang-format off
	return elektraPluginExport ("template",
		ELEKTRA_PLUGIN_OPEN,	&elektraTemplateOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTemplateClose,
		ELEKTRA_PLUGIN_GET,	&elektraTemplateGet,
		ELEKTRA_PLUGIN_SET,	&elektraTemplateSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraTemplateError,
		ELEKTRA_PLUGIN_END);
}

