/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NETWORK_H
#define ELEKTRA_PLUGIN_NETWORK_H

#include <elektra/kdb/errors.h>
#include <elektra/plugin/plugin.h>

#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

int elektraNetworkAddrInfo (Key * toCheck);

int elektraPortInfo (Key * toCheck, Key * parentKey);

int elektraNetworkGet (Plugin * handle, KeySet * ks, Key * parentKey);

int elektraNetworkSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
