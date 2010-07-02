/***************************************************************************
          timeofday.c  -  Skeleton of a plugin to be copied
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libelektra.so a valid plugin.                             *
 *   Simple fill the empty _timeofday functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "timeofday.h"

#ifndef timersub
#define timersub(val1,val2,res) \
	if (1) \
	{ \
		(res)->tv_sec = (val1)->tv_sec - (val2)->tv_sec; \
		if (((res)->tv_usec = (val1)->tv_usec - (val2)->tv_usec) < 0) \
		{ \
			(res)->tv_sec --; \
			(res)->tv_usec += 1000000; \
		} \
	}
#endif

static char* timeofday(char *t, struct timeval *start)
{
	struct timeval now;
	struct timeval tv;

	gettimeofday(&now, 0);

	timersub (&now, start, &tv);

	for (int i=9; i>=4; --i)
	{
		t[i] = tv.tv_usec %10 + '0';
		tv.tv_usec /= 10;
	}
	for (int i=3; i>=0; --i)
	{
		t[i] = tv.tv_sec %10 + '0';
		tv.tv_sec /= 10;
	}
	t[10] = 0;
	return t;
}

int elektraTimeofdayOpen(Plugin *handle, Key *k)
{
	struct timeval *start = malloc(sizeof (struct timeval));
	char t[10];

	gettimeofday(start, 0);
	elektraPluginSetData(handle, start);

	fprintf(stderr, "open\t%s\n", timeofday(t, start));

	return 0; /* success */
}

int elektraTimeofdayClose(Plugin *handle, Key *k)
{
	char t[10];
	struct timeval *start = elektraPluginGetData(handle);

	fprintf(stderr, "close\t%s\n", timeofday(t, start));

	free(start);

	return 0; /* success */
}

int elektraTimeofdayGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	char t[10];
	struct timeval *start = elektraPluginGetData(handle);

	fprintf(stderr, "get\t%s\n", timeofday(t, start));

	Key *root = keyNew("system/elektra/modules/timeofday", KEY_END);
	if (keyRel (root, parentKey) >= 0)
	{
		ksAppend (returned, ksNew (30,
			keyNew ("system/elektra/modules/timeofday",
				KEY_VALUE, "timeofday plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/timeofday/exports", KEY_END),
			keyNew ("system/elektra/modules/timeofday/exports/open",
				KEY_SIZE, sizeof (&elektraTimeofdayOpen),
				KEY_BINARY,
				KEY_VALUE, &elektraTimeofdayOpen, KEY_END),
			keyNew ("system/elektra/modules/timeofday/exports/close",
				KEY_SIZE, sizeof (&elektraTimeofdayClose),
				KEY_BINARY,
				KEY_VALUE, &elektraTimeofdayClose, KEY_END),
			keyNew ("system/elektra/modules/timeofday/exports/get",
				KEY_SIZE, sizeof (&elektraTimeofdayGet),
				KEY_BINARY,
				KEY_VALUE, &elektraTimeofdayGet, KEY_END),
			keyNew ("system/elektra/modules/timeofday/exports/set",
				KEY_SIZE, sizeof (&elektraTimeofdaySet),
				KEY_BINARY,
				KEY_VALUE, &elektraTimeofdaySet, KEY_END),
			keyNew ("system/elektra/modules/timeofday/exports/error",
				KEY_SIZE, sizeof (&elektraTimeofdayError),
				KEY_BINARY,
				KEY_VALUE, &elektraTimeofdayError, KEY_END),
			keyNew ("system/elektra/modules/timeofday/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/timeofday/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/timeofday/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/timeofday/infos/description",
				KEY_VALUE, "Prints timestamps when a method is called", KEY_END),
			keyNew ("system/elektra/modules/timeofday/infos/provides",
				KEY_VALUE, "timeofday", KEY_END),
			keyNew ("system/elektra/modules/timeofday/infos/placements",
				KEY_VALUE, "prerollback postrollback pregetstorage postgetstorage presetstorage precommit postcommit", KEY_END),
			keyNew ("system/elektra/modules/timeofday/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/timeofday/infos/version",
				KEY_VALUE, "1.0", KEY_END),
			KS_END));

			fprintf(stderr, "fin\t%s\n", timeofday(t, start));
	}

	keyDel (root);
	return nr_keys; /* success */
}

int elektraTimeofdaySet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	char t[10];
	struct timeval *start = elektraPluginGetData(handle);

	fprintf(stderr, "set\t%s\n", timeofday(t, start));

	return nr_keys;
}

int elektraTimeofdayError(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ssize_t nr_keys = 0;
	char t[10];
	struct timeval *start = elektraPluginGetData(handle);

	fprintf(stderr, "err\t%s\n", timeofday(t, start));

	return nr_keys;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(timeofday)
{
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_OPEN,	&elektraTimeofdayOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTimeofdayClose,
		ELEKTRA_PLUGIN_GET,	&elektraTimeofdayGet,
		ELEKTRA_PLUGIN_SET,	&elektraTimeofdaySet,
		ELEKTRA_PLUGIN_ERROR,	&elektraTimeofdayError,
		ELEKTRA_PLUGIN_END);
}

