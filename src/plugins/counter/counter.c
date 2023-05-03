/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/config.h>
#include <internal/macros/attributes.h>
#include <internal/macros/plugin_errors.h>

#include <stdio.h>
#include <string.h>

#include "./counter.h"

typedef int Counter;
#define COUNTER_FMT "%d"

static Counter elektraCountOpen = 0;

int elektraCounterOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	elektraCountOpen += 1;
	KeySet * config = elektraPluginGetConfig (handle);
	if (ksLookupByName (config, "/module", 0))
	{
		if (ksLookupByName (config, "/logmodule", 0))
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

int elektraCounterClose (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	elektraCountClose += 1;
	KeySet * config = elektraPluginGetConfig (handle);
	if (ksLookupByName (config, "/module", 0))
	{
		if (ksLookupByName (config, "/logmodule", 0))
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

int elektraCounterGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/counter"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/counter", KEY_VALUE, "counter plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/counter/exports", KEY_END),
			       keyNew ("system:/elektra/modules/counter/exports/open", KEY_FUNC, elektraCounterOpen, KEY_END),
			       keyNew ("system:/elektra/modules/counter/exports/close", KEY_FUNC, elektraCounterClose, KEY_END),
			       keyNew ("system:/elektra/modules/counter/exports/get", KEY_FUNC, elektraCounterGet, KEY_END),
			       keyNew ("system:/elektra/modules/counter/exports/set", KEY_FUNC, elektraCounterSet, KEY_END),
			       keyNew ("system:/elektra/modules/counter/exports/error", KEY_FUNC, elektraCounterError, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/counter/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	return 1; /* success */
}

int elektraCounterSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */

	return 1; /* success */
}

int elektraCounterError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
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

