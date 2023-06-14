/**
 * @file
 *
 * @brief A plugin which informs the user about a missing backend.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./missing.h"

#include <elektra/core.h>
#include <elektra/core/errors.h>
#include <elektra/plugin/plugin.h>

#include <internal/macros/attributes.h>
#include <internal/utility/compare.h>

int elektraMissingGet (Plugin * plugin ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/missing"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/missing", KEY_VALUE, "The missing plugin is waiting for your orders", KEY_END),
			keyNew ("system:/elektra/modules/missing/exports", KEY_END),
			keyNew ("system:/elektra/modules/missing/exports/get", KEY_FUNC, elektraMissingGet, KEY_END),
			keyNew ("system:/elektra/modules/missing/exports/set", KEY_FUNC, elektraMissingSet, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/missing/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Tried to get a key from a missing backend: %s", keyName (parentKey));
	return -1;
}

int elektraMissingSet (Plugin * plugin ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "Tried to set a key from a missing backend: %s", keyName (parentKey));
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
