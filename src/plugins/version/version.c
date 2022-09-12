/**
 * @file
 *
 * @brief Source for version plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "version.h"

#include <kdbprivate.h> // for keyReplacePrefix
#include <kdbversion.h>

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * definition ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// init as read-only
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/version"))
	{
		ElektraKeyset * contract =
			ksNew (30, keyNew ("system:/elektra/modules/version", KEY_VALUE, "version plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/version/exports", KEY_END),
			       keyNew ("system:/elektra/modules/version/exports/init", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (init), KEY_END),
			       keyNew ("system:/elektra/modules/version/exports/get", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/version/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	const char * phase = elektraPluginGetPhase (handle);
	if (strcmp (phase, KDB_GET_PHASE_RESOLVER) == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else if (strcmp (phase, KDB_GET_PHASE_STORAGE) == 0)
	{
		ElektraKeyset * info = elektraVersionKeySet ();
		ElektraKey * versionRoot = keyNew ("system:/elektra/version", KEY_END);

		ElektraKey * first = keyDup (ksAtCursor (info, 0), KEY_CP_ALL);
		keyReplacePrefix (first, versionRoot, parentKey);
		keySetMeta (first, "restrict/write", "1");
		keySetMeta (first, "restrict/remove", "1");
		ksAppendKey (returned, first);

		for (elektraCursor i = 1; i < ksGetSize (info); i++)
		{
			ElektraKey * cur = keyDup (ksAtCursor (info, i), KEY_CP_ALL);
			keyReplacePrefix (cur, versionRoot, parentKey);
			keyCopyAllMeta (cur, first);
			ksAppendKey (returned, cur);
		}

		ksDel (info);
		keyDel (versionRoot);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("version",
		ELEKTRA_PLUGIN_INIT,    &ELEKTRA_PLUGIN_FUNCTION(init),
		ELEKTRA_PLUGIN_GET,	&ELEKTRA_PLUGIN_FUNCTION(get),
		ELEKTRA_PLUGIN_END);
	// clang-format on
}
