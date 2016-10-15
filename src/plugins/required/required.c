/**
 * @file
 *
 * @brief Source for required plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "required.h"

#include <kdberrors.h>
#include <kdbhelper.h>

int elektraRequiredGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/required"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/required", KEY_VALUE, "required plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/required/exports", KEY_END),
			       keyNew ("system/elektra/modules/required/exports/get", KEY_FUNC, elektraRequiredGet, KEY_END),
			       keyNew ("system/elektra/modules/required/exports/set", KEY_FUNC, elektraRequiredSet, KEY_END),
#include ELEKTRA_README (required)
			       keyNew ("system/elektra/modules/required/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys

	return 1; // success
}

int elektraRequiredSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ksRewind (returned);
	Key * cur = 0;
	while ((cur = ksNext (returned)))
	{
		const Key * meta = keyGetMeta (cur, "required");
		if (!meta)
		{
			meta = keyGetMeta (cur, "mandatory");
			if (!meta)
			{
				keyRewindMeta (cur);
				int c = 0;
				while (keyNextMeta (cur))
				{
					++c;
				}
				ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_REQUIRED, parentKey, "key '%s' with %d metadata is not required",
						    keyName (cur), c);
				return -1; // error
			}
		}
	}

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (required)
{
	// clang-format off
	return elektraPluginExport ("required",
		ELEKTRA_PLUGIN_GET,	&elektraRequiredGet,
		ELEKTRA_PLUGIN_SET,	&elektraRequiredSet,
		ELEKTRA_PLUGIN_END);
}

