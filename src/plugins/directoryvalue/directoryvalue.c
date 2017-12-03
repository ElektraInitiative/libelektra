/**
 * @file
 *
 * @brief Source for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "directoryvalue.h"

#include <kdbhelper.h>

int elektraDirectoryvalueGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/directoryvalue"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/directoryvalue", KEY_VALUE,
					   "directoryvalue plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/directoryvalue/exports", KEY_END),
			       keyNew ("system/elektra/modules/directoryvalue/exports/get", KEY_FUNC, elektraDirectoryvalueGet, KEY_END),
			       keyNew ("system/elektra/modules/directoryvalue/exports/set", KEY_FUNC, elektraDirectoryvalueSet, KEY_END),
#include ELEKTRA_README (directoryvalue)
			       keyNew ("system/elektra/modules/directoryvalue/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraDirectoryvalueSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (directoryvalue)
{
	return elektraPluginExport ("directoryvalue", ELEKTRA_PLUGIN_GET, &elektraDirectoryvalueGet, ELEKTRA_PLUGIN_SET,
				    &elektraDirectoryvalueSet, ELEKTRA_PLUGIN_END);
}
