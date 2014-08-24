/***************************************************************************
                     network.c  -  Skeleton of a plugin
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
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "network.h"

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

/* Obtain address(es) matching host/port */
int elektraNetworkAddrInfo(Key *toCheck)
{
	struct addrinfo *result;
	int s;

	const Key *meta = keyGetMeta (toCheck, "check/ipaddr");

	if (!meta) return 0; /* No check to do */

	struct addrinfo hints;
	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;     /* Allow IPv4 or IPv6 */
	hints.ai_socktype = SOCK_DGRAM;  /* Datagram socket */
	hints.ai_flags = AI_NUMERICHOST; /* Only accept numeric hosts */
	hints.ai_protocol = 0;           /* Any protocol */

	s = getaddrinfo(keyString(toCheck), keyString(meta), &hints, &result);

	if (s != 0) {
		return s;
	}

	freeaddrinfo(result);

	return 0;
}

int elektraNetworkGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey ELEKTRA_UNUSED)
{
	/* configuration only */
	KeySet *n;
	ksAppend (returned, n=ksNew (30,
		keyNew ("system/elektra/modules/network",
			KEY_VALUE, "network plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/network/exports", KEY_END),
		keyNew ("system/elektra/modules/network/exports/get",
			KEY_FUNC, elektraNetworkGet,
			KEY_END),
		keyNew ("system/elektra/modules/network/exports/set",
			KEY_FUNC, elektraNetworkSet,
			KEY_END),
		keyNew ("system/elektra/modules/network/exports/elektraNetworkAddrInfo",
			KEY_FUNC, elektraNetworkAddrInfo,
			KEY_END),
#include "readme_network.c"
		keyNew ("system/elektra/modules/network/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraNetworkSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* set all keys */
	Key *cur;
	int s;

	ksRewind (returned);
	while ((cur = ksNext(returned)) != 0)
	{
		s = elektraNetworkAddrInfo(cur);
		if (s != 0)
		{
			const char *gaimsg = gai_strerror(s);
			char *errmsg = malloc (strlen (gaimsg)
					+ keyGetNameSize(cur)
					+ keyGetValueSize(cur)
					+ sizeof ("name:  value:  message: "));
			strcpy (errmsg, "name: ");
			strcat (errmsg, keyName(cur));
			strcat (errmsg, " value: ");
			strcat (errmsg, keyValue(cur));
			strcat (errmsg, " message: ");
			strcat (errmsg, gaimsg);
			ELEKTRA_SET_ERROR (51, parentKey, errmsg);
			free (errmsg);
			return -1;
		}
	}

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(network)
{
	return elektraPluginExport("network",
		ELEKTRA_PLUGIN_GET,	&elektraNetworkGet,
		ELEKTRA_PLUGIN_SET,	&elektraNetworkSet,
		ELEKTRA_PLUGIN_END);
}

