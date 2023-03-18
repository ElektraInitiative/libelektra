/**
 * @file
 *
 * @brief Source for the toml plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <elektra/kdb.h>
#include <elektra/kdb/errors.h>
#include <internal/macros/utils.h>
#include <internal/utility/logger.h>

#include "driver.h"
#include "toml.h"
#include "write.h"

KeySet * getContract (void)
{
	return ksNew (30, keyNew ("system:/elektra/modules/toml", KEY_VALUE, "toml plugin waits for your orders", KEY_END),
		      keyNew ("system:/elektra/modules/toml/exports", KEY_END),
		      keyNew ("system:/elektra/modules/toml/exports/get", KEY_FUNC, elektraTomlGet, KEY_END),
		      keyNew ("system:/elektra/modules/toml/exports/set", KEY_FUNC, elektraTomlSet, KEY_END),
#include ELEKTRA_README
		      keyNew ("system:/elektra/modules/toml/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}


int elektraTomlGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (strcmp (keyName (parentKey), "system:/elektra/modules/toml") == 0)
	{
		KeySet * contract = getContract ();
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else
	{
		int result = tomlRead (returned, parentKey);
		return result == 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}

int elektraTomlSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	int result = tomlWrite (returned, parentKey);
	return result == 0 ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("toml", ELEKTRA_PLUGIN_GET, &elektraTomlGet, ELEKTRA_PLUGIN_SET, &elektraTomlSet, ELEKTRA_PLUGIN_END);
}
