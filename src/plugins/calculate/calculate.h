/**
* @file
*
* @brief Header for calculate plugin
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#ifndef ELEKTRA_PLUGIN_CALCULATE_H
#define ELEKTRA_PLUGIN_CALCULATE_H

#include <kdbplugin.h>


int elektraCalculateGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCalculateSet(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(calculate);

#endif
