/**
 * @file
 *
 * @brief Header for len plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LENGTH_H
#define ELEKTRA_PLUGIN_LENGTH_H

#include <kdbplugin.h>

int elektraLengthGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraLengthSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
