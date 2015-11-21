/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_LINE_H
#define ELEKTRA_PLUGIN_LINE_H

#include <kdbplugin.h>


int elektraLineGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraLineSet(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(line);

#endif
