/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <stdio.h>
#include <string.h>

#include "counter.h"

typedef int Counter;
#define COUNTER_FMT "%d"

static Counter elektraCountOpen = 0;

int elektraCounterOpen (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	elektraCountOpen += 1;
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	if (elektraKeysetLookupByName (config, "/module", 0))
	{
		if (elektraKeysetLookupByName (config, "/logmodule", 0))
		{
			printf ("%p elektraCounterOpen  (module) called " COUNTER_FMT " times\n", (void *) handle, elektraCountOpen);
		}
	}
	else
	{
		printf ("%p elektraCounterOpen           called " COUNTER_FMT " times\n", (void *) handle, elektraCountOpen);
	}

	return 1; /* success */
}

static Counter elektraCountClose = 0;

int elektraCounterClose (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	elektraCountClose += 1;
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	if (elektraKeysetLookupByName (config, "/module", 0))
	{
		if (elektraKeysetLookupByName (config, "/logmodule", 0))
		{
			printf ("%p elektraCounterClose (module) called " COUNTER_FMT " times\n", (void *) handle, elektraCountClose);
		}
	}
	else
	{
		printf ("%p elektraCounterClose          called " COUNTER_FMT " times\n", (void *) handle, elektraCountClose);
	}

	return 1; /* success */
}

int elektraCounterGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/counter"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/counter", ELEKTRA_KEY_VALUE, "counter plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/counter/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/counter/exports/open", ELEKTRA_KEY_FUNC, elektraCounterOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/counter/exports/close", ELEKTRA_KEY_FUNC, elektraCounterClose, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/counter/exports/get", ELEKTRA_KEY_FUNC, elektraCounterGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/counter/exports/set", ELEKTRA_KEY_FUNC, elektraCounterSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/counter/exports/error", ELEKTRA_KEY_FUNC, elektraCounterError, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/counter/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	return 1; /* success */
}

int elektraCounterSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

int elektraCounterError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("counter",
		ELEKTRA_PLUGIN_OPEN,	&elektraCounterOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCounterClose,
		ELEKTRA_PLUGIN_GET,	&elektraCounterGet,
		ELEKTRA_PLUGIN_SET,	&elektraCounterSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraCounterError,
		ELEKTRA_PLUGIN_END);
}

