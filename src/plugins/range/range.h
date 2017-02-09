/**
 * @file
 *
 * @brief Header for range plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_RANGE_H
#define ELEKTRA_PLUGIN_RANGE_H

#include <kdbplugin.h>


int elektraRangeOpen (Plugin * handle, Key * errorKey);
int elektraRangeClose (Plugin * handle, Key * errorKey);
int elektraRangeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRangeSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRangeError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRangeCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (range);

#endif
