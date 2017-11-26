/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_COUNTER_H
#define ELEKTRA_PLUGIN_COUNTER_H

#include <kdbplugin.h>


int elektraCounterOpen (Plugin * handle, Key * errorKey);
int elektraCounterClose (Plugin * handle, Key * errorKey);
int elektraCounterGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCounterSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCounterError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (counter);

#endif
