/**
 * @file
 *
 * @brief A plugin which informs the user about a missing backend.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "missing.h"
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbplugin.h>


int elektraMissingGet (Plugin * plugin ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/missing"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/missing", ELEKTRA_KEY_VALUE, "The missing plugin is waiting for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/missing/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/missing/exports/get", ELEKTRA_KEY_FUNC, elektraMissingGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/missing/exports/set", ELEKTRA_KEY_FUNC, elektraMissingSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/missing/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Tried to get a key from a missing backend: %s", elektraKeyName (parentKey));
	return -1;
}

int elektraMissingSet (Plugin * plugin ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Tried to set a key from a missing backend: %s", elektraKeyName (parentKey));
	return -1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("missing",
		ELEKTRA_PLUGIN_GET,	&elektraMissingGet,
		ELEKTRA_PLUGIN_SET,	&elektraMissingSet,
		ELEKTRA_PLUGIN_END);
}
