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


#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include "syslog.h"

int elektraSyslogOpen(Plugin *handle, Key *parentKey ELEKTRA_UNUSED)
{
	/* plugin initialization logic */

	if (!ksLookupByName(elektraPluginGetConfig(handle), "/dontopensyslog", 0))
	{
		openlog ("elektra", LOG_PID, LOG_USER);
	}

	return 0; /* success */
}

int elektraSyslogClose(Plugin *handle, Key *parentKey ELEKTRA_UNUSED)
{
	/* free all plugin resources and shut it down */

	if (!ksLookupByName(elektraPluginGetConfig(handle), "/dontopensyslog", 0))
	{
		closelog();
	}

	return 0; /* success */
}

int elektraSyslogGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	KeySet *n;
	ksAppend (returned, n = ksNew (30,
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
#include "readme_syslog.c"
		keyNew ("system/elektra/modules/syslog/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	ksDel (n);
	return 1;
}

int elektraSyslogSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	size_t changed = 0;
	Key *k=0;
	ksRewind(returned);
	while ((k = ksNext(returned)))
	{
		if (keyNeedSync(k))
		{
			syslog(LOG_NOTICE, "change %s to %s", keyName(k), keyString(k));
			changed ++;
		}
	}

	syslog (LOG_NOTICE, "committed configuration %s with %zd keys (%zd changed)",
			keyName(parentKey),
			ksGetSize(returned),
			changed);

	return 1;
}

int elektraSyslogError(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
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

