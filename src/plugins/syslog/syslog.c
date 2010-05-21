/***************************************************************************
          syslog.c  -  Skeleton of a plugin to be copied
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
 *   Simple fill the empty _syslog functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "syslog.h"

int kdbOpen_syslog(Plugin *handle)
{
	/* plugin initialization logic */

	if (!ksLookupByName(pluginGetConfig(handle), "/dontopensyslog", 0))
	{
		openlog ("elektra", LOG_PID, LOG_USER);
	}

	return 0; /* success */
}

int kdbClose_syslog(Plugin *handle)
{
	/* free all plugin resources and shut it down */

	return 0; /* success */
}

ssize_t kdbGet_syslog(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;

	syslog (LOG_NOTICE, "get configuration %s with %zd keys",
			keyName(parentKey),
			ksGetSize(returned));

	return nr_keys; /* success */
}

ssize_t kdbSet_syslog(Plugin *handle, KeySet *returned, const Key *parentKey)
{
	ssize_t nr_keys = 0;

	syslog (LOG_NOTICE, "set configuration %s with %zd keys",
			keyName(parentKey),
			ksGetSize(returned));

	return nr_keys;
}

Plugin *KDBEXPORT(syslog)
{
	return pluginExport(BACKENDNAME,
		KDB_PLUGIN_OPEN,	&kdbOpen_syslog,
		KDB_PLUGIN_CLOSE,	&kdbClose_syslog,
		KDB_PLUGIN_GET,		&kdbGet_syslog,
		KDB_PLUGIN_SET,		&kdbSet_syslog,
		KDB_PLUGIN_VERSION,	BACKENDVERSION,
		KDB_PLUGIN_AUTHOR,	"Markus Raab <elektra@markus-raab.org>",
		KDB_PLUGIN_LICENCE,	"BSD",
		KDB_PLUGIN_DESCRIPTION,	"Logs get and set calls to syslog",
		KDB_PLUGIN_NEEDS,	"",
		KDB_PLUGIN_PROVIDES,	"",
		KDB_PLUGIN_END);
}

