/**
 * @file
 *
 * @brief Source for the toml plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <kdb.h>
#include <kdberrors.h>
#include <kdblogger.h>
#include <kdbmacros.h>

#include "driver.h"
#include "toml.h"
#include "write.h"

ElektraKeyset * getContract (void)
{
	return elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/toml", ELEKTRA_KEY_VALUE, "toml plugin waits for your orders", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/toml/exports", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/toml/exports/get", ELEKTRA_KEY_FUNC, elektraTomlGet, ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/modules/toml/exports/set", ELEKTRA_KEY_FUNC, elektraTomlSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
		      elektraKeyNew ("system:/elektra/modules/toml/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
}


int elektraTomlGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (strcmp (elektraKeyName (parentKey), "system:/elektra/modules/toml") == 0)
	{
		ElektraKeyset * contract = getContract ();
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else
	{
		int result = tomlRead (returned, parentKey);
		return result == 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}

int elektraTomlSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	int result = tomlWrite (returned, parentKey);
	return result == 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("toml", ELEKTRA_PLUGIN_GET, &elektraTomlGet, ELEKTRA_PLUGIN_SET, &elektraTomlSet, ELEKTRA_PLUGIN_END);
}
