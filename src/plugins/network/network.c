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

#define NETWORK_ATTEMPTS 3
#define NETWORK_SECONDS_BETWEEN_ATTEMPTS 5

int elektraPortInfo (Key * toCheck, Key * parentKey)
{
	const Key * meta = keyGetMeta (toCheck, "check/port");
	const Key * listenMeta = keyGetMeta (toCheck, "check/port/listen");
	if (!meta && !listenMeta) return 0; /* No check to do */
	char * endptr = NULL;
	long portNumber = strtol (keyString (toCheck), &endptr, 10);
	int portNumberNetworkByteOrder;

	if (*endptr == '\0')
	{
		if (portNumber < 0 || portNumber > 65535)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Port %ld on key %s was not within 0 - 65535", portNumber,
								keyName (toCheck));
			return -1;
		}
		portNumberNetworkByteOrder = htons (portNumber);
	}
	else
	{
		struct servent * service;
		service = getservbyname (keyString (toCheck), NULL); // NULL means we accept both tcp and udp
		if (service == NULL)
		{
			// `getservbyname` does not set any errno
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Could not find service with name %s on key %s",
								keyString (toCheck), keyName (toCheck));
			return -1;
		}
		portNumberNetworkByteOrder = service->s_port;
	}

	if (!listenMeta) return 0; /* No check to do */

	char const * hostname = "localhost";

	int sockfd;
	struct sockaddr_in serv_addr;
	struct hostent * server;
	sockfd = socket (AF_INET, SOCK_STREAM, 0);

	if (sockfd < 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not open a socket. Reason: %s", strerror (errno));
	}

	for (unsigned int attempt = 1, seconds = NETWORK_SECONDS_BETWEEN_ATTEMPTS; attempt <= NETWORK_ATTEMPTS;
	     attempt++, seconds *= NETWORK_SECONDS_BETWEEN_ATTEMPTS)
	{
		server = gethostbyname (hostname);
		if (server == NULL)
		{
			if (errno == HOST_NOT_FOUND)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Could not connect to %s: No such host", hostname);
				return -1;
			}
			else
			{
				if (errno == TRY_AGAIN && attempt < NETWORK_ATTEMPTS)
				{
					ELEKTRA_LOG_DEBUG ("Could not connect, %d retries left, wait for next attempt...",
							   NETWORK_ATTEMPTS - attempt);
					sleep (seconds);
					ELEKTRA_LOG_DEBUG ("Done with waiting, continue");
				}
				else
				{
					ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
						parentKey, "There was an error trying to connect to host '%s'. Reason: %s", hostname,
						strerror (errno));
					return -1;
				}
			}
		}
	}

	bzero ((char *) &serv_addr, sizeof (serv_addr));
	serv_addr.sin_family = AF_INET;
	bcopy ((char *) server->h_addr, (char *) &serv_addr.sin_addr.s_addr, server->h_length);

	serv_addr.sin_port = (in_port_t) portNumberNetworkByteOrder;
	if (bind (sockfd, (struct sockaddr *) &serv_addr, sizeof (serv_addr)) < 0)
	{
		close (sockfd);
		if (errno == EADDRINUSE)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Port %s is already in use which was specified on key %s",
								keyString (toCheck), keyName (toCheck));
		}
		else
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey,
								"Could not bind to port %s which was specified on key %s. Reason: %s",
								keyString (toCheck), keyName (toCheck), strerror (errno));
		}
		return -1;
	}
	close (sockfd);

	return 0;
}

int elektraNetworkGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	/* configuration only */
	KeySet * n;
	ksAppend (returned,
		  n = ksNew (30, keyNew ("system:/elektra/modules/network", KEY_VALUE, "network plugin waits for your orders", KEY_END),
			     keyNew ("system:/elektra/modules/network/exports", KEY_END),
			     keyNew ("system:/elektra/modules/network/exports/get", KEY_FUNC, elektraNetworkGet, KEY_END),
			     keyNew ("system:/elektra/modules/network/exports/set", KEY_FUNC, elektraNetworkSet, KEY_END),
			     keyNew ("system:/elektra/modules/network/exports/elektraNetworkAddrInfo", KEY_FUNC, elektraNetworkAddrInfo,
				     KEY_END),
			     keyNew ("system:/elektra/modules/network/exports/elektraPortInfo", KEY_FUNC, elektraNetworkAddrInfo, KEY_END),

#include "readme_network.c"

			     keyNew ("system:/elektra/modules/network/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraNetworkSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	/* check all keys */
	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
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
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (parentKey, errmsg);
			elektraFree (errmsg);
			return -1;
		}
		int p = elektraPortInfo (cur, parentKey);
		if (p != 0)
		{
			return -1;
		}
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("network",
				    ELEKTRA_PLUGIN_GET, &elektraNetworkGet,
				    ELEKTRA_PLUGIN_SET, &elektraNetworkSet,
				    ELEKTRA_PLUGIN_END);
}

