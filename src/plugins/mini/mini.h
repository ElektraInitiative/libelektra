/**
 * @file
 *
 * @brief Header for mini plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MINI_H
#define ELEKTRA_PLUGIN_MINI_H

#include <kdbplugin.h>


int elektraMiniGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraMiniSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
