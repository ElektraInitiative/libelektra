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

#include "tracer.h"

#include <string.h>

int elektraTracerOpen (Plugin * handle, Key * errorKey)
{
	ssize_t nr_keys = 0;
	KeySet * config = elektraPluginGetConfig (handle);

	if (ksLookupByName (config, "/module", 0))
	{
		if (ksLookupByName (config, "/logmodule", 0))
		{
			Key * k;
			printf ("tracer: openmodule(%p, %s = %s): ", (void *) handle, keyName (errorKey), keyString (errorKey));
			while ((k = ksNext (config)) != 0)
			{
				printf ("%s=%s ", keyName (k), keyString (k));
				++nr_keys;
			}
			printf ("%zd\n", nr_keys);
		}
	}
	else
	{
		Key * k;
		printf ("tracer: open(%p, %s = %s): ", (void *) handle, keyName (errorKey), keyString (errorKey));
		while ((k = ksNext (config)) != 0)
		{
			printf ("%s=%s ", keyName (k), keyString (k));
			++nr_keys;
		}
		printf ("%zd\n", nr_keys);
	}


	return 0;
}

int elektraTracerClose (Plugin * handle, Key * errorKey)
{
	KeySet * config = elektraPluginGetConfig (handle);

	if (ksLookupByName (config, "/module", 0))
	{
		if (ksLookupByName (config, "/logmodule", 0))
		{
			printf ("tracer: closemodule(%p, %s = %s)\n", (void *) handle, keyName (errorKey), keyString (errorKey));
		}
	}
	else
	{
		printf ("tracer: close(%p, %s = %s)\n", (void *) handle, keyName (errorKey), keyString (errorKey));
	}

	return 0;
}

int elektraTracerGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ssize_t nr_keys = 0;
	Key * k = 0;

	if (!strcmp (keyName (parentKey), "system/elektra/modules/tracer"))
	{
		KeySet * info =
			ksNew (50, keyNew ("system/elektra/modules/tracer", KEY_VALUE, "tracer plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/tracer/exports", KEY_END),
			       keyNew ("system/elektra/modules/tracer/exports/open", KEY_FUNC, elektraTracerOpen, KEY_END),
			       keyNew ("system/elektra/modules/tracer/exports/close", KEY_FUNC, elektraTracerClose, KEY_END),
			       keyNew ("system/elektra/modules/tracer/exports/get", KEY_FUNC, elektraTracerGet, KEY_END),
			       keyNew ("system/elektra/modules/tracer/exports/set", KEY_FUNC, elektraTracerSet, KEY_END),
			       keyNew ("system/elektra/modules/tracer/exports/error", KEY_FUNC, elektraTracerError, KEY_END),
#include "readme_tracer.c"
			       keyNew ("system/elektra/modules/tracer/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, info);
		ksDel (info);
		return 1;
	}

	printf ("tracer: get(%p, %s, %s): ", (void *) handle, keyName (parentKey), keyString (parentKey));
	while ((k = ksNext (returned)) != 0)
	{
		printf ("%s ", keyName (k));
		++nr_keys;
	}
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

int elektraTracerSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ssize_t nr_keys = 0;
	Key * k = 0;

	printf ("tracer: set(%p, %s, %s): ", (void *) handle, keyName (parentKey), keyString (parentKey));
	while ((k = ksNext (returned)) != 0)
	{
		printf ("%s ", keyName (k));
		++nr_keys;
	}
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

int elektraTracerError (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ssize_t nr_keys = 0;
	Key * k = 0;

	printf ("tracer: error(%p, %s, %s): ", (void *) handle, keyName (parentKey), keyString (parentKey));
	while ((k = ksNext (returned)) != 0)
	{
		printf ("%s ", keyName (k));
		++nr_keys;
	}
	printf ("%zd\n", nr_keys);

	return nr_keys;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("tracer",
		ELEKTRA_PLUGIN_OPEN,	&elektraTracerOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTracerClose,
		ELEKTRA_PLUGIN_GET,	&elektraTracerGet,
		ELEKTRA_PLUGIN_SET,	&elektraTracerSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraTracerError,
		ELEKTRA_PLUGIN_END);
}

