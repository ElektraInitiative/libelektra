/**
 * @file
 *
 * @brief Source for iterate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./iterate.h"

#include <internal/utility/compare.h>
int elektraIterateOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic

	return 1; // success
}

int elektraIterateClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down

	return 1; // success
}

static int doIterate (KeySet * returned)
{
	int ret = 0;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * k = ksAtCursor (returned, it);
		const Key * m = keyGetMeta (k, "iterate");
		if (m)
		{
			ret = 1;
		}
	}

	return ret;
}

int elektraIterateGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/iterate"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/iterate", KEY_VALUE, "iterate plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports", KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/open", KEY_FUNC, elektraIterateOpen, KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/close", KEY_FUNC, elektraIterateClose, KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/get", KEY_FUNC, elektraIterateGet, KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/set", KEY_FUNC, elektraIterateSet, KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/error", KEY_FUNC, elektraIterateError, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/iterate/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	return doIterate (returned);
}

int elektraIterateSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	return doIterate (returned);
}

int elektraIterateError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("iterate",
		ELEKTRA_PLUGIN_OPEN,	&elektraIterateOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraIterateClose,
		ELEKTRA_PLUGIN_GET,	&elektraIterateGet,
		ELEKTRA_PLUGIN_SET,	&elektraIterateSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraIterateError,
		ELEKTRA_PLUGIN_END);
}

