/**
 * @file
 *
 * @brief Header for lineendings plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LINEENDINGS_H
#define ELEKTRA_PLUGIN_LINEENDINGS_H

#include <kdbplugin.h>


int elektraLineendingsGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraLineendingsSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
