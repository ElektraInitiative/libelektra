/**
 * @file
 *
 * @brief Source for version plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "version.h"

#include <internal/kdbprivate.h> // for keyReplacePrefix

static inline KeySet * elektraVersionKeySet (void)
{
	return ksNew (50,
		      keyNew ("system:/elektra/version", KEY_VALUE,
			      "Below are version information of the Elektra Library you are currently using", KEY_END),
		      keyNew ("system:/elektra/version/constants", KEY_END),
		      keyNew ("system:/elektra/version/constants/KDB_VERSION", KEY_VALUE, KDB_VERSION, KEY_END),
		      keyNew ("system:/elektra/version/constants/KDB_VERSION_MAJOR", KEY_VALUE, KDB_VERSION_MAJOR, KEY_END),
		      keyNew ("system:/elektra/version/constants/KDB_VERSION_MINOR", KEY_VALUE, KDB_VERSION_MINOR, KEY_END),
		      keyNew ("system:/elektra/version/constants/KDB_VERSION_PATCH", KEY_VALUE, KDB_VERSION_PATCH, KEY_END),
		      keyNew ("system:/elektra/version/constants/SO_VERSION", KEY_VALUE, SO_VERSION, KEY_END),
		      keyNew ("system:/elektra/version/infos", KEY_VALUE, "All information you want to know", KEY_END),
		      keyNew ("system:/elektra/version/infos/author", KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		      keyNew ("system:/elektra/version/infos/licence", KEY_VALUE, "BSD", KEY_END),
		      keyNew ("system:/elektra/version/infos/description", KEY_VALUE, "Information of your Elektra Installation", KEY_END),
		      keyNew ("system:/elektra/version/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle ELEKTRA_UNUSED, KeySet * definition ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// init as read-only
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/version"))
	{
		KeySet * contract =
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

	switch (elektraPluginGetPhase (handle))
	{
	case ELEKTRA_KDB_GET_PHASE_RESOLVER:
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	case ELEKTRA_KDB_GET_PHASE_STORAGE: {
		KeySet * info = elektraVersionKeySet ();
		Key * versionRoot = keyNew ("system:/elektra/version", KEY_END);

		Key * first = keyDup (ksAtCursor (info, 0), KEY_CP_ALL);
		keyReplacePrefix (first, versionRoot, parentKey);
		keySetMeta (first, "restrict/write", "1");
		keySetMeta (first, "restrict/remove", "1");
		ksAppendKey (returned, first);

		for (elektraCursor i = 1; i < ksGetSize (info); i++)
		{
			Key * cur = keyDup (ksAtCursor (info, i), KEY_CP_ALL);
			keyReplacePrefix (cur, versionRoot, parentKey);
			keyCopyAllMeta (cur, first);
			ksAppendKey (returned, cur);
		}

		ksDel (info);
		keyDel (versionRoot);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	default:
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
