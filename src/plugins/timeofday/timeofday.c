/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/utility/old_helper.h>

#include "./timeofday.h"

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

int elektraTimeofdayOpen (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	TimeofdayInfo * ti = calloc (1, sizeof (TimeofdayInfo));
	char t[ARRAY_LENGTH];

	elektraPluginSetData (handle, ti);

	// init time
	gettimeofday (&ti->start, 0);
	ti->last = ti->start;

	KeySet * config = elektraPluginGetConfig (handle);
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

int elektraTimeofdayClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	char t[ARRAY_LENGTH];
	TimeofdayInfo * ti = elektraPluginGetData (handle);

	KeySet * config = elektraPluginGetConfig (handle);
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

int elektraTimeofdayGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	char t[ARRAY_LENGTH];
	TimeofdayInfo * ti = elektraPluginGetData (handle);

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/timeofday"))
	{
		KeySet * pluginConfig = ksNew (
			30, keyNew ("system:/elektra/modules/timeofday", KEY_VALUE, "timeofday plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports", KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/open", KEY_FUNC, elektraTimeofdayOpen, KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/close", KEY_FUNC, elektraTimeofdayClose, KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/get", KEY_FUNC, elektraTimeofdayGet, KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/set", KEY_FUNC, elektraTimeofdaySet, KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/commit", KEY_FUNC, elektraTimeofdayCommit, KEY_END),
			keyNew ("system:/elektra/modules/timeofday/exports/error", KEY_FUNC, elektraTimeofdayError, KEY_END),
#include "./readme_timeofday.c"
			keyNew ("system:/elektra/modules/timeofday/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, pluginConfig);
		ksDel (pluginConfig);

		KeySet * config = elektraPluginGetConfig (handle);
		if (ksLookupByName (config, "/logmodule", 0))
		{
			fprintf (stderr, "get\t%s\tpos\t%s\n", elektraTimeofdayHelper (t, ti), "postmodulesconf");
		}

		return 1;
	}

	const char * position;
	switch (elektraPluginGetPhase (handle))
	{
	case ELEKTRA_KDB_GET_PHASE_PRE_STORAGE:
		position = "get/prestorage";
		break;
	case ELEKTRA_KDB_GET_PHASE_POST_STORAGE:
		position = "get/poststorage";
		break;
	default:
		position = "get/???";
		break;
	}

	fprintf (stderr, "get\t%s\tpos\t%s\n", elektraTimeofdayHelper (t, ti), position);

	return 1;
}

int elektraTimeofdaySet (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	char t[ARRAY_LENGTH];
	TimeofdayInfo * ti = elektraPluginGetData (handle);
	const char * position;

	switch (elektraPluginGetPhase (handle))
	{
	case ELEKTRA_KDB_SET_PHASE_PRE_STORAGE:
		position = "set/prestorage";
		break;
	default:
		position = "set/???";
		break;
	}
	fprintf (stderr, "set\t%s\tpos\t%s\n", elektraTimeofdayHelper (t, ti), position);

	return 1;
}

int elektraTimeofdayCommit (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	char t[ARRAY_LENGTH];
	TimeofdayInfo * ti = elektraPluginGetData (handle);
	const char * position;

	switch (elektraPluginGetPhase (handle))
	{
	case ELEKTRA_KDB_SET_PHASE_PRE_COMMIT:
		position = "set/precommit";
		break;
	case ELEKTRA_KDB_SET_PHASE_POST_COMMIT:
		position = "set/postcommit";
		break;
	default:
		position = "set/???";
		break;
	}

	fprintf (stderr, "commit\t%s\tpos\t%s\n", elektraTimeofdayHelper (t, ti), position);

	return 1;
}

int elektraTimeofdayError (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	char t[ARRAY_LENGTH];
	TimeofdayInfo * ti = elektraPluginGetData (handle);
	const char * position;

	switch (elektraPluginGetPhase (handle))
	{
	case ELEKTRA_KDB_SET_PHASE_PRE_ROLLBACK:
		position = "set/prerollback";
		break;
	case ELEKTRA_KDB_SET_PHASE_POST_ROLLBACK:
		position = "set/postrollback";
		break;
	default:
		position = "set/???";
		break;
	}
	fprintf (stderr, "error\t%s\tpos\t%s\n", elektraTimeofdayHelper (t, ti), position);

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
		ELEKTRA_PLUGIN_COMMIT,	&elektraTimeofdayCommit,
		ELEKTRA_PLUGIN_ERROR,	&elektraTimeofdayError,
		ELEKTRA_PLUGIN_END);
}

