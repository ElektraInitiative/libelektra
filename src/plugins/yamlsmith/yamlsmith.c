/**
 * @file
 *
 * @brief Source for yamlsmith plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "yamlsmith.h"

#include <kdbhelper.h>

int elektraYamlsmithGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/yamlsmith"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/yamlsmith", KEY_VALUE, "yamlsmith plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/yamlsmith/exports", KEY_END),
			keyNew ("system/elektra/modules/yamlsmith/exports/get", KEY_FUNC, elektraYamlsmithGet, KEY_END),
			keyNew ("system/elektra/modules/yamlsmith/exports/set", KEY_FUNC, elektraYamlsmithSet, KEY_END),
#include ELEKTRA_README (yamlsmith)
			keyNew ("system/elektra/modules/yamlsmith/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraYamlsmithSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yamlsmith)
{
	return elektraPluginExport ("yamlsmith", ELEKTRA_PLUGIN_GET, &elektraYamlsmithGet, ELEKTRA_PLUGIN_SET, &elektraYamlsmithSet,
				    ELEKTRA_PLUGIN_END);
}
