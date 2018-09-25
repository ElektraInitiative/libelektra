/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "network.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

/* Obtain address(es) matching host/port */
int elektraNetworkAddrInfo (Key * toCheck)
{
	struct addrinfo * result;
	int s;

	const Key * meta = keyGetMeta (toCheck, "check/ipaddr");

	if (!meta) return 0; /* No check to do */

	struct addrinfo hints;
	memset (&hints, 0, sizeof (struct addrinfo));
	hints.ai_family = AF_UNSPEC; /* Allow IPv4 or IPv6 */
	if (!strcmp (keyString (meta), "ipv4"))
	{
		hints.ai_family = AF_INET;
		hints.ai_flags = AI_NUMERICHOST; /* Only accept numeric hosts */
	}
	else if (!strcmp (keyString (meta), "ipv6"))
	{
		hints.ai_family = AF_INET6;
		hints.ai_flags = AI_NUMERICHOST; /* Only accept numeric hosts */
	}
	hints.ai_socktype = SOCK_DGRAM; /* Datagram socket */
	hints.ai_protocol = 0;		/* Any protocol */

	s = getaddrinfo (keyString (toCheck), NULL, &hints, &result);

	if (s != 0)
	{
		return s;
	}

	freeaddrinfo (result);

	return 0;
}

int elektraPortInfo(Key * toCheck, Key * parentKey) {
	const Key * meta = keyGetMeta (toCheck, "check/port");
	if (!meta) return 0; /* No check to do */
	char *endptr = NULL;
	long portNumber = strtol(keyString(toCheck), &endptr, 10);

	if (*endptr == '\0') {
		if (portNumber < 0 || portNumber > 65535 || *endptr != 0) {
			ELEKTRA_SET_ERRORF(201, parentKey, "Port %d on key %s was not within 0 - 65535",
							   portNumber, keyName(toCheck));
			return -1;
		}
	} else {
		ELEKTRA_LOG("Is String");
		struct servent* service;
		service = getservbyname(keyString(toCheck), NULL); //NULL means we accept both tcp and udp
		if (service == NULL) {
			ELEKTRA_SET_ERRORF(202, parentKey, "Could not find service with name %s on key %s",
							   keyString(toCheck), keyName(toCheck));
			return -1;
		}
		ELEKTRA_LOG("Was a valid Service");
	}

	//strtol returns 0 if the port was actually invalid
	//since 0 is a valid port we need to check explicitly if


	return 0;
}

int elektraNetworkGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	/* configuration only */
	KeySet * n;
	ksAppend (returned,
		  n = ksNew (30, keyNew ("system/elektra/modules/network", KEY_VALUE, "network plugin waits for your orders", KEY_END),
			     keyNew ("system/elektra/modules/network/exports", KEY_END),
			     keyNew ("system/elektra/modules/network/exports/get", KEY_FUNC, elektraNetworkGet, KEY_END),
			     keyNew ("system/elektra/modules/network/exports/set", KEY_FUNC, elektraNetworkSet, KEY_END),
			     keyNew ("system/elektra/modules/network/exports/elektraNetworkAddrInfo", KEY_FUNC, elektraNetworkAddrInfo,
				     KEY_END),
				 keyNew ("system/elektra/modules/network/exports/elektraPortInfo", KEY_FUNC, elektraNetworkAddrInfo,
					 KEY_END),
#include "readme_network.c"
			     keyNew ("system/elektra/modules/network/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraNetworkSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	/* check all keys */
	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		int s = elektraNetworkAddrInfo (cur);
		if (s != 0)
		{
			const char * gaimsg = gai_strerror (s);
			char * errmsg = elektraMalloc (strlen (gaimsg) + keyGetNameSize (cur) + keyGetValueSize (cur) +
						       sizeof ("name:  value:  message: "));
			strcpy (errmsg, "name: ");
			strcat (errmsg, keyName (cur));
			strcat (errmsg, " value: ");
			strcat (errmsg, keyValue (cur));
			strcat (errmsg, " message: ");
			strcat (errmsg, gaimsg);
			ELEKTRA_SET_ERROR (51, parentKey, errmsg);
			elektraFree (errmsg);
			return -1;
		}
		int p = elektraPortInfo(cur, parentKey);
		if (p != 0) {
			ELEKTRA_SET_ERROR(51, parentKey, "Port failed");
			return -1;
		}
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (network)
{
	// clang-format off
	return elektraPluginExport("network",
		ELEKTRA_PLUGIN_GET,	&elektraNetworkGet,
		ELEKTRA_PLUGIN_SET,	&elektraNetworkSet,
		ELEKTRA_PLUGIN_END);
}

