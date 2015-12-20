/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NETWORK_H
#define ELEKTRA_PLUGIN_NETWORK_H

#include <kdbplugin.h>
#include <kdberrors.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

int elektraNetworkAddrInfo(Key *toCheck);

int elektraNetworkGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraNetworkSet(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(network);

#endif
