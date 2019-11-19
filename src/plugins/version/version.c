/**
 * @file
 *
 * @brief Source for version plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "version.h"

#include <kdbprivate.h>
#include <kdbversion.h>
#include <kdberrors.h>


int elektraVersionGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/version"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/version", KEY_VALUE, "version plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/version/exports", KEY_END),
			       keyNew ("system/elektra/modules/version/exports/get", KEY_FUNC, elektraVersionGet, KEY_END),
			       keyNew ("system/elektra/modules/version/exports/set", KEY_FUNC, elektraVersionSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/version/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys
	KeySet * info = elektraVersionKeySet ();

	keySetMeta (info->array[0], "restrict/write", "1");
	keySetMeta (info->array[0], "restrict/remove", "1");
	for (size_t i = 1; i < info->size; i++)
	{
		keyCopyAllMeta (info->array[i], info->array[0]);
	}
	ksAppend (returned, info);
	ksDel (info);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraVersionSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	KeySet * info = elektraVersionKeySet ();
	ELEKTRA_SET_ERROR_READ_ONLY (info, returned, parentKey);

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
	{
		// clang-format off
		return elektraPluginExport ("version",
		ELEKTRA_PLUGIN_GET,	&elektraVersionGet,
		ELEKTRA_PLUGIN_SET,	&elektraVersionSet,
		ELEKTRA_PLUGIN_END);
	}
