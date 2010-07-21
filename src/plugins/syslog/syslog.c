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

int elektraSyslogOpen(Plugin *handle, Key *parent)
{
	/* plugin initialization logic */

	if (!ksLookupByName(elektraPluginGetConfig(handle), "/dontopensyslog", 0))
	{
		openlog ("elektra", LOG_PID, LOG_USER);
	}

	return 0; /* success */
}

int elektraSyslogClose(Plugin *handle, Key *parent)
{
	/* free all plugin resources and shut it down */

	if (!ksLookupByName(elektraPluginGetConfig(handle), "/dontopensyslog", 0))
	{
		closelog();
	}

	return 0; /* success */
}

int elektraSyslogGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	ksAppend (returned, ksNew (30,
		keyNew ("system/elektra/modules/syslog",
			KEY_VALUE, "syslog plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/syslog/exports", KEY_END),
		keyNew ("system/elektra/modules/syslog/exports/open",
			KEY_FUNC, elektraSyslogOpen,
			KEY_END),
		keyNew ("system/elektra/modules/syslog/exports/close",
			KEY_FUNC, elektraSyslogClose,
			KEY_END),
		keyNew ("system/elektra/modules/syslog/exports/get",
			KEY_FUNC, elektraSyslogGet,
			KEY_END),
		keyNew ("system/elektra/modules/syslog/exports/set",
			KEY_FUNC, elektraSyslogSet,
			KEY_END),
		keyNew ("system/elektra/modules/syslog/exports/error",
			KEY_FUNC, elektraSyslogError,
			KEY_END),
		keyNew ("system/elektra/modules/syslog/infos",
			KEY_VALUE, "All information you want to know", KEY_END),
		keyNew ("system/elektra/modules/syslog/infos/author",
			KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
		keyNew ("system/elektra/modules/syslog/infos/licence",
			KEY_VALUE, "BSD", KEY_END),
		keyNew ("system/elektra/modules/syslog/infos/description",
			KEY_VALUE, "Logs set and error calls to syslog", KEY_END),
		keyNew ("system/elektra/modules/syslog/infos/provides",
			KEY_VALUE, "filter", KEY_END),
		keyNew ("system/elektra/modules/syslog/infos/placements",
			KEY_VALUE, "postcommit postrollback", KEY_END),
		keyNew ("system/elektra/modules/syslog/infos/needs",
			KEY_VALUE, "", KEY_END),
		keyNew ("system/elektra/modules/syslog/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	return 1;
}

int elektraSyslogSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	syslog (LOG_NOTICE, "committed configuration %s with %zd keys",
			keyName(parentKey),
			ksGetSize(returned));

	return 1;
}

int elektraSyslogError(Plugin *handle, KeySet *returned, Key *parentKey)
{
	syslog (LOG_NOTICE, "rollback configuration %s with %zd keys",
			keyName(parentKey),
			ksGetSize(returned));

	return 1;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(syslog)
{
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_OPEN,	&elektraSyslogOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraSyslogClose,
		ELEKTRA_PLUGIN_GET,	&elektraSyslogGet,
		ELEKTRA_PLUGIN_SET,	&elektraSyslogSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraSyslogError,
		ELEKTRA_PLUGIN_END);
}

