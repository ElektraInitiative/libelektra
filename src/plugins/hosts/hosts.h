/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_HOSTS_H
#define ELEKTRA_PLUGIN_HOSTS_H

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <elektra/kdb.h>
#include <elektra/kdbplugin.h>
#include <kdberrors.h>

#include <stdlib.h>

#define HOSTS_KDB_BUFFER_SIZE 16384
/*Test size for small buffer
#define HOSTS_KDB_BUFFER_SIZE 16 */

#define MAX_ORDER_SIZE 50


int elektraHostsGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHostsSet (Plugin * handle, KeySet * ks, Key * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
