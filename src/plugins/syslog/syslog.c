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

	if (!ksLookupByName(elektraPluginGetConfig(handle), "/dontopensyslog", 0))
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

Plugin *ELEKTRA_PLUGIN_EXPORT(syslog)
{
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_OPEN,	&kdbOpen_syslog,
		ELEKTRA_PLUGIN_CLOSE,	&kdbClose_syslog,
		ELEKTRA_PLUGIN_GET,		&kdbGet_syslog,
		ELEKTRA_PLUGIN_SET,		&kdbSet_syslog,
		ELEKTRA_PLUGIN_VERSION,	BACKENDVERSION,
		ELEKTRA_PLUGIN_AUTHOR,	"Markus Raab <elektra@markus-raab.org>",
		ELEKTRA_PLUGIN_LICENCE,	"BSD",
		ELEKTRA_PLUGIN_DESCRIPTION,	"Logs get and set calls to syslog",
		ELEKTRA_PLUGIN_NEEDS,	"",
		ELEKTRA_PLUGIN_PROVIDES,	"",
		ELEKTRA_PLUGIN_END);
}

