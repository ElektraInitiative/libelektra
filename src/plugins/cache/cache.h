/**
 * @file
 *
 * @brief Header for cache plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CACHE_H
#define ELEKTRA_PLUGIN_CACHE_H

#include <kdbplugin.h>


int elektraCacheOpen (Plugin * handle, ElektraKey * errorKey);
int elektraCacheClose (Plugin * handle, ElektraKey * errorKey);
int elektraCacheGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCacheSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
