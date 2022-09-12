/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NETWORK_H
#define ELEKTRA_PLUGIN_NETWORK_H

#include <kdberrors.h>
#include <kdbplugin.h>

#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

int elektraNetworkAddrInfo (ElektraKey * toCheck);

int elektraPortInfo (ElektraKey * toCheck, ElektraKey * parentKey);

int elektraNetworkGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

int elektraNetworkSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
