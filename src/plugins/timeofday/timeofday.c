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

static char* timeofday(char *t)
{
	struct timeval tv;

	gettimeofday(&tv, 0);
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

int kdbOpen_timeofday(Plugin *handle)
{
	char t[10];
	fprintf(stderr, "open %s\n", timeofday(t));

	return 0; /* success */
}

int kdbClose_timeofday(Plugin *handle)
{
	char t[10];
	fprintf(stderr, "close %s\n", timeofday(t));

	return 0; /* success */
}

ssize_t kdbGet_timeofday(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	char t[10];

	fprintf(stderr, "get %s\n", timeofday(t));

	return nr_keys; /* success */
}

ssize_t kdbSet_timeofday(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;
	char t[10];

	fprintf(stderr, "set %s\n", timeofday(t));

	return nr_keys;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(timeofday)
{
	return elektraPluginExport(BACKENDNAME,
		KDB_PLUGIN_OPEN,	&kdbOpen_timeofday,
		KDB_PLUGIN_CLOSE,	&kdbClose_timeofday,
		KDB_PLUGIN_GET,		&kdbGet_timeofday,
		KDB_PLUGIN_SET,		&kdbSet_timeofday,
		KDB_PLUGIN_VERSION,	BACKENDVERSION,
		KDB_PLUGIN_AUTHOR,	"Full Name <email@libelektra.org>",
		KDB_PLUGIN_LICENCE,	"BSD",
		KDB_PLUGIN_DESCRIPTION,	"Add description here",
		KDB_PLUGIN_NEEDS,	"",
		KDB_PLUGIN_PROVIDES,	"",
		KDB_PLUGIN_END);
}

