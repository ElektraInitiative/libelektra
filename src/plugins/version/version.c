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
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/version"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/version", ELEKTRA_KEY_VALUE, "version plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/version/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/version/exports/init", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (init), ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/version/exports/get", ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/version/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	const char * phase = elektraPluginGetPhase (handle);
	if (strcmp (phase, ELEKTRA_KDB_GET_PHASE_RESOLVER) == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else if (strcmp (phase, ELEKTRA_KDB_GET_PHASE_STORAGE) == 0)
	{
		ElektraKeyset * info = elektraVersionKeySet ();
		ElektraKey * versionRoot = elektraKeyNew ("system:/elektra/version", ELEKTRA_KEY_END);

		ElektraKey * first = elektraKeyDup (elektraKeysetAtCursor (info, 0), ELEKTRA_KEY_CP_ALL);
		elektraKeyReplacePrefix (first, versionRoot, parentKey);
		elektraKeySetMeta (first, "restrict/write", "1");
		elektraKeySetMeta (first, "restrict/remove", "1");
		elektraKeysetAppendKey (returned, first);

		for (elektraCursor i = 1; i < elektraKeysetGetSize (info); i++)
		{
			ElektraKey * cur = elektraKeyDup (elektraKeysetAtCursor (info, i), ELEKTRA_KEY_CP_ALL);
			elektraKeyReplacePrefix (cur, versionRoot, parentKey);
			elektraKeyCopyAllMeta (cur, first);
			elektraKeysetAppendKey (returned, cur);
		}

		elektraKeysetDel (info);
		elektraKeyDel (versionRoot);

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
