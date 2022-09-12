/**
 * @file
 *
 * @brief Source for iterate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "iterate.h"

#include <kdbhelper.h>


int elektraIterateOpen (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic

	return 1; // success
}

int elektraIterateClose (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down

	return 1; // success
}

static int doIterate (ElektraKeyset * returned)
{
	int ret = 0;
	ElektraKey * k;
	while ((k = ksNext (returned)))
	{
		const ElektraKey * m = keyGetMeta (k, "iterate");
		if (m)
		{
			ret = 1;
		}
	}

	return ret;
}

int elektraIterateGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/iterate"))
	{
		ElektraKeyset * contract =
			ksNew (30, keyNew ("system:/elektra/modules/iterate", ELEKTRA_KEY_VALUE, "iterate plugin waits for your orders", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/open", ELEKTRA_KEY_FUNC, elektraIterateOpen, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/close", ELEKTRA_KEY_FUNC, elektraIterateClose, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/get", ELEKTRA_KEY_FUNC, elektraIterateGet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/set", ELEKTRA_KEY_FUNC, elektraIterateSet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/iterate/exports/error", ELEKTRA_KEY_FUNC, elektraIterateError, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/iterate/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	return doIterate (returned);
}

int elektraIterateSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	return doIterate (returned);
}

int elektraIterateError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
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

