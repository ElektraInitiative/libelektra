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


int elektraCounterOpen (Plugin * handle, ElektraKey * errorKey);
int elektraCounterClose (Plugin * handle, ElektraKey * errorKey);
int elektraCounterGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCounterSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCounterError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
