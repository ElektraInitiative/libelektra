/**
 * @file
 *
 * @brief Header for range plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_RANGE_H
#define ELEKTRA_PLUGIN_RANGE_H

#define RANGE_ERROR_MESSAGE "Value '%s' of key '%s' not within range %s"
#include <kdbplugin.h>


int elektraRangeGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraRangeSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
