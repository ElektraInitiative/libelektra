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
int elektraNetworkAddrInfo (ElektraKey * toCheck)
{
	struct addrinfo * result;
	int s;

	const ElektraKey * meta = elektraKeyGetMeta (toCheck, "check/ipaddr");

	if (!meta) return 0; /* No check to do */

	struct addrinfo hints;
	memset (&hints, 0, sizeof (struct addrinfo));
	hints.ai_family = AF_UNSPEC; /* Allow IPv4 or IPv6 */
	if (!strcmp (elektraKeyString (meta), "ipv4"))
	{
		hints.ai_family = AF_INET;
		hints.ai_flags = AI_NUMERICHOST; /* Only accept numeric hosts */
	}
	else if (!strcmp (elektraKeyString (meta), "ipv6"))
	{
		hints.ai_family = AF_INET6;
		hints.ai_flags = AI_NUMERICHOST; /* Only accept numeric hosts */
	}
	hints.ai_socktype = SOCK_DGRAM; /* Datagram socket */
	hints.ai_protocol = 0;		/* Any protocol */

	s = getaddrinfo (elektraKeyString (toCheck), NULL, &hints, &result);

	if (s != 0)
	{
		return s;
	}

	freeaddrinfo (result);

	return 0;
}

int elektraPortInfo (ElektraKey * toCheck, ElektraKey * parentKey)
{
	const ElektraKey * meta = elektraKeyGetMeta (toCheck, "check/port");
	const ElektraKey * listenMeta = elektraKeyGetMeta (toCheck, "check/port/listen");
	if (!meta && !listenMeta) return 0; /* No check to do */
	char * endptr = NULL;
	long portNumber = strtol (elektraKeyString (toCheck), &endptr, 10);
	int portNumberNetworkByteOrder;

	if (*endptr == '\0')
	{
		if (portNumber < 0 || portNumber > 65535)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Port %ld on key %s was not within 0 - 65535", portNumber,
								elektraKeyName (toCheck));
			return -1;
		}
		portNumberNetworkByteOrder = htons (portNumber);
	}
	else
	{
		struct servent * service;
		service = getservbyname (elektraKeyString (toCheck), NULL); // NULL means we accept both tcp and udp
		if (service == NULL)
		{
			// `getservbyname` does not set any errno
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Could not find service with name %s on key %s",
								elektraKeyString (toCheck), elektraKeyName (toCheck));
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
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "There was an error trying to connect to host '%s'. Reason: %s",
								hostname, strerror (errno));
			return -1;
		}
		// TODO: Maybe consider errno == TRY_AGAIN separately and try to reconnect
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
								elektraKeyString (toCheck), elektraKeyName (toCheck));
		}
		else
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey,
								"Could not bind to port %s which was specified on key %s. Reason: %s",
								elektraKeyString (toCheck), elektraKeyName (toCheck), strerror (errno));
		}
		return -1;
	}
	close (sockfd);

	return 0;
}

int elektraNetworkGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	/* configuration only */
	ElektraKeyset * n;
	elektraKeysetAppend (returned,
		  n = elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/network", ELEKTRA_KEY_VALUE, "network plugin waits for your orders", ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/network/exports", ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/network/exports/get", ELEKTRA_KEY_FUNC, elektraNetworkGet, ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/network/exports/set", ELEKTRA_KEY_FUNC, elektraNetworkSet, ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/network/exports/elektraNetworkAddrInfo", ELEKTRA_KEY_FUNC, elektraNetworkAddrInfo,
				     ELEKTRA_KEY_END),
			     elektraKeyNew ("system:/elektra/modules/network/exports/elektraPortInfo", ELEKTRA_KEY_FUNC, elektraNetworkAddrInfo, ELEKTRA_KEY_END),

#include "readme_network.c"

			     elektraKeyNew ("system:/elektra/modules/network/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END));
	elektraKeysetDel (n);

	return 1; /* success */
}

int elektraNetworkSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	/* check all keys */
	ElektraKey * cur;
	elektraKeysetRewind (returned);
	while ((cur = elektraKeysetNext (returned)) != 0)
	{
		int s = elektraNetworkAddrInfo (cur);
		if (s != 0)
		{
			const char * gaimsg = gai_strerror (s);
			char * errmsg = elektraMalloc (strlen (gaimsg) + elektraKeyGetNameSize (cur) + elektraKeyGetValueSize (cur) +
						       sizeof ("name:  value:  message: "));
			strcpy (errmsg, "name: ");
			strcat (errmsg, elektraKeyName (cur));
			strcat (errmsg, " value: ");
			strcat (errmsg, elektraKeyValue (cur));
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

