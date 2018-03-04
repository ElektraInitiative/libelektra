/**
 * @file
 *
 * @brief Source for antlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "antlr.h"

#include <kdbhelper.h>

int elektraAntlrGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/antlr"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/antlr", KEY_VALUE, "antlr plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/antlr/exports", KEY_END),
			       keyNew ("system/elektra/modules/antlr/exports/get", KEY_FUNC, elektraAntlrGet, KEY_END),
			       keyNew ("system/elektra/modules/antlr/exports/set", KEY_FUNC, elektraAntlrSet, KEY_END),
#include ELEKTRA_README (antlr)
			       keyNew ("system/elektra/modules/antlr/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraAntlrSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (antlr)
{
	return elektraPluginExport ("antlr", ELEKTRA_PLUGIN_GET, &elektraAntlrGet, ELEKTRA_PLUGIN_SET, &elektraAntlrSet,
				    ELEKTRA_PLUGIN_END);
}
