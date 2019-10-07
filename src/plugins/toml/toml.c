/**
  1  * @file
  2  *
  3  * @brief Source for the toml plugin
  4  *
  5  * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
  6  *
  7  */


#include <kdb.h>
#include <kdberrors.h>
#include <kdbmacros.h>

#include "toml.h"

KeySet * getContract ()
{
	return ksNew (30, keyNew ("system/elektra/modules/toml", KEY_VALUE, "toml plugin waits for your orders", KEY_END),
		      keyNew ("system/elektra/modules/toml/exports", KEY_END),
		      keyNew ("system/elektra/modules/toml/exports/get", KEY_FUNC, elektraTomlGet, KEY_END),
		      keyNew ("system/elektra/modules/toml/exports/set", KEY_FUNC, elektraTomlSet, KEY_END),
#include ELEKTRA_README
		      keyNew ("system/elektra/modules/toml/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}


int elektraTomlGet (Plugin * handle, ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{

	if (strcmp (keyName (parentKey), "system/elektra/modules/toml") == 0)
	{
		KeySet * contract = getContract ();
		ksAppend (returned, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	// TODO Read from toml file (filename = parent key as string?)
	status = ELEKTRA_PLUGIN_STATUS_SUCCESS;

	return status;
}

int elektraTomlSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// TODO Write to toml file (filename = parent key as string?)

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("toml", ELEKTRA_PLUGIN_GET, &elektraTomlGet, ELEKTRA_PLUGIN_SET, &elektraTomlSet, ELEKTRA_PLUGIN_END);
}
