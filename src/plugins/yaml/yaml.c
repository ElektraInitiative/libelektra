/**
 * @file
 *
 * @brief Source for yaml plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include "yaml.h"

#include <kdbhelper.h>

int elektraYamlGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/yaml"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/yaml", KEY_VALUE, "yaml plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/yaml/exports", KEY_END),
			       keyNew ("system/elektra/modules/yaml/exports/get", KEY_FUNC, elektraYamlGet, KEY_END),
			       keyNew ("system/elektra/modules/yaml/exports/set", KEY_FUNC, elektraYamlSet, KEY_END),
#include ELEKTRA_README (yaml)
			       keyNew ("system/elektra/modules/yaml/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraYamlSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (yaml)
{
	// clang-format off
	return elektraPluginExport ("yaml",
		ELEKTRA_PLUGIN_GET,	&elektraYamlGet,
		ELEKTRA_PLUGIN_SET,	&elektraYamlSet,
		ELEKTRA_PLUGIN_END);
}
