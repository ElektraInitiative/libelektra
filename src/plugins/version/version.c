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

int elektraVersionGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/version"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/version", KEY_VALUE, "version plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/version/exports", KEY_END),
			       keyNew ("system:/elektra/modules/version/exports/get", KEY_FUNC, elektraVersionGet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/version/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

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

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("version",
		ELEKTRA_PLUGIN_GET,	&elektraVersionGet,
		ELEKTRA_PLUGIN_END);
	// clang-format on
}
