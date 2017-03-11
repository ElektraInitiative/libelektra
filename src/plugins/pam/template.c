/**
 * @file
 *
 * @brief Source for template plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "template.h"

#include <kdbhelper.h>


int elektraTemplateGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/auth"))
	{
		printf("auth: %s", keyName (parentKey));
		return 1; // success
	}

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
