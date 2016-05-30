/**
 * @file
 *
 * @brief Header for cachefilter plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CACHEFILTER_H
#define ELEKTRA_PLUGIN_CACHEFILTER_H

#include <kdbplugin.h>


int elektraCachefilterGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCachefilterSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCachefilterClose (Plugin * handle, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (cachefilter);

#endif
