/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include "hidden.h"

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <string.h>

int elektraHiddenOpen(Plugin *handle, Key *errorKey ELEKTRA_UNUSED)
{
	elektraPluginSetData(handle, ksNew(0, KS_END));

	return 1; /* success */
}

int elektraHiddenClose(Plugin *handle, Key *errorKey ELEKTRA_UNUSED)
{
	ksDel (elektraPluginGetData(handle));

	return 1; /* success */
}

int elektraHiddenGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	if (!strcmp (keyName(parentKey), "system/elektra/modules/hidden"))
	{
		KeySet *pluginConfig = ksNew (30,
			keyNew ("system/elektra/modules/hidden",
				KEY_VALUE, "hidden plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/hidden/exports", KEY_END),
			keyNew ("system/elektra/modules/hidden/exports/open",
				KEY_FUNC, elektraHiddenOpen,
				KEY_END),
			keyNew ("system/elektra/modules/hidden/exports/close",
				KEY_FUNC, elektraHiddenClose,
				KEY_END),
			keyNew ("system/elektra/modules/hidden/exports/get",
				KEY_FUNC, elektraHiddenGet,
				KEY_END),
			keyNew ("system/elektra/modules/hidden/exports/set",
				KEY_FUNC, elektraHiddenSet,
				KEY_END),
#include "readme_hidden.c"
			keyNew ("system/elektra/modules/hidden/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned,pluginConfig);
		ksDel (pluginConfig);
		return 1;
	}

	Key *cur = 0;
	KeySet *newReturned = ksNew (ksGetSize (returned), KS_END);
	KeySet *hiddenKeys = elektraPluginGetData (handle);
	ksClear (hiddenKeys);

	while ((cur = ksNext(returned)) != 0)
	{
		if (keyIsInactive(cur)) ksAppendKey (newReturned, cur);
		else ksAppendKey (hiddenKeys, cur);
	}

	ksCopy (returned, newReturned);
	ksDel (newReturned);

	return 1; /* success */
}

int elektraHiddenSet(Plugin *handle, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	KeySet *hiddenKeys = elektraPluginGetData (handle);
	ksAppend (returned, hiddenKeys);

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(hidden)
{
	// clang-format off
	return elektraPluginExport("hidden",
		ELEKTRA_PLUGIN_OPEN,	&elektraHiddenOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraHiddenClose,
		ELEKTRA_PLUGIN_GET,	&elektraHiddenGet,
		ELEKTRA_PLUGIN_SET,	&elektraHiddenSet,
		ELEKTRA_PLUGIN_END);
}

