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

#include <elektra/plugin/plugin.h>


int elektraCacheOpen (Plugin * handle, Key * errorKey);
int elektraCacheClose (Plugin * handle, Key * errorKey);
int elektraCacheGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCacheSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
