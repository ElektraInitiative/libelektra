/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbhelper.h>

#include "timeofday.h"

#include <string.h>

#define ARRAY_LENGTH 25

static inline void timeofday (char * t, struct timeval * start, struct timeval * now)
{
	struct timeval tv;

	tv.tv_sec = now->tv_sec - start->tv_sec;
	if ((tv.tv_usec = now->tv_usec - start->tv_usec) < 0)
	{
		tv.tv_sec--;
		tv.tv_usec += 1000000;
	}

	for (int i = 9; i >= 4; --i)
	{
		t[i] = tv.tv_usec % 10 + '0';
		tv.tv_usec /= 10;
	}
	for (int i = 3; i >= 0; --i)
	{
		t[i] = tv.tv_sec % 10 + '0';
		tv.tv_sec /= 10;
	}
	t[10] = 0;
}

const char * elektraTimeofdayHelper (char * t, TimeofdayInfo * ti)
{
	struct timeval now;
	gettimeofday (&now, 0);
	timeofday (t, &ti->start, &now);
	t[10] = '\t';
	t[11] = 'd';
	t[12] = 'i';
	t[13] = '\t';
	timeofday (&t[14], &ti->last, &now);
	ti->last = now;

	return t;
}

int elektraTimeofdayOpen (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	TimeofdayInfo * ti = calloc (1, sizeof (TimeofdayInfo));
	char t[ARRAY_LENGTH];

	elektraPluginSetData (handle, ti);

	// init time
	gettimeofday (&ti->start, 0);
	ti->last = ti->start;

	ElektraKeyset * config = elektraPluginGetConfig (handle);
	if (ksLookupByName (config, "/module", 0))
	{
		if (ksLookupByName (config, "/logmodule", 0))
		{
			fprintf (stderr, "open (module)\t%s\n", elektraTimeofdayHelper (t, ti));
		}
	}
	else
	{
		fprintf (stderr, "open\t%s\n", elektraTimeofdayHelper (t, ti));
	}

	return 0; /* success */
}

int elektraTimeofdayClose (Plugin * handle, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	char t[ARRAY_LENGTH];
	TimeofdayInfo * ti = elektraPluginGetData (handle);

	ElektraKeyset * config = elektraPluginGetConfig (handle);
	if (ksLookupByName (config, "/module", 0))
	{
		if (ksLookupByName (config, "/logmodule", 0))
		{
			fprintf (stderr, "close (module)\t%s\n", elektraTimeofdayHelper (t, ti));
		}
	}
	else
	{
		fprintf (stderr, "close\t%s\n", elektraTimeofdayHelper (t, ti));
	}

	elektraFree (ti);

	return 0; /* success */
}

int elektraTimeofdayGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	char t[ARRAY_LENGTH];
	TimeofdayInfo * ti = elektraPluginGetData (handle);
	const char * position = "get";

	ti->nrset = 0;
	++ti->nrget;
	if (ti->nrget == 1)
		position = "pregetstorage";
	else if (ti->nrget == 2)
	{
		ti->nrget = 0;
		position = "postgetstorage";
	}

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/timeofday"))
	{
		ElektraKeyset * pluginConfig = ksNew (
			30, keyNew ("system:/elektra/modules/timeofday", ELEKTRA_KEY_VALUE, "timeofday plugin waits for your orders", ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports", ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/open", ELEKTRA_KEY_FUNC, elektraTimeofdayOpen, ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/close", ELEKTRA_KEY_FUNC, elektraTimeofdayClose, ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/get", ELEKTRA_KEY_FUNC, elektraTimeofdayGet, ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/set", ELEKTRA_KEY_FUNC, elektraTimeofdaySet, ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/error", ELEKTRA_KEY_FUNC, elektraTimeofdayError, ELEKTRA_KEY_END),
#include "readme_timeofday.c"
			keyNew ("system:/elektra/modules/timeofday/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, pluginConfig);
		ksDel (pluginConfig);

		ElektraKeyset * config = elektraPluginGetConfig (handle);
		if (ksLookupByName (config, "/logmodule", 0))
		{
			fprintf (stderr, "get\t%s\tpos\t%s\n", elektraTimeofdayHelper (t, ti), "postmodulesconf");
		}

		return 1;
	}

	fprintf (stderr, "get\t%s\tpos\t%s\n", elektraTimeofdayHelper (t, ti), position);

	return 1;
}

int elektraTimeofdaySet (Plugin * handle, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	char t[ARRAY_LENGTH];
	TimeofdayInfo * ti = elektraPluginGetData (handle);
	const char * position = "set";

	ti->nrget = 0;
	++ti->nrset;
	if (ti->nrset == 1)
		position = "presetstorage";
	else if (ti->nrset == 2)
		position = "precommit";
	else if (ti->nrset == 3)
	{
		ti->nrset = 0;
		position = "postcommit";
	}

	fprintf (stderr, "set\t%s\tpos\t%s\n", elektraTimeofdayHelper (t, ti), position);

	return 1;
}

int elektraTimeofdayError (Plugin * handle, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	char t[ARRAY_LENGTH];
	TimeofdayInfo * ti = elektraPluginGetData (handle);
	const char * position = "error";

	ti->nrset = 0;
	ti->nrget = 0;
	++ti->nrerr;
	if (ti->nrerr == 1)
		position = "prerollback";
	else if (ti->nrerr == 2)
	{
		ti->nrerr = 0;
		position = "postrollback";
	}

	fprintf (stderr, "err\t%s\tpos\t%s\n", elektraTimeofdayHelper (t, ti), position);

	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_OPEN,	&elektraTimeofdayOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTimeofdayClose,
		ELEKTRA_PLUGIN_GET,	&elektraTimeofdayGet,
		ELEKTRA_PLUGIN_SET,	&elektraTimeofdaySet,
		ELEKTRA_PLUGIN_ERROR,	&elektraTimeofdayError,
		ELEKTRA_PLUGIN_END);
}

