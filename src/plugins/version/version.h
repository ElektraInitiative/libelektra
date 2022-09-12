/**
 * @file
 *
 * @brief Header for version plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_VERSION_H
#define ELEKTRA_PLUGIN_VERSION_H

#include <kdbplugin.h>

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle, ElektraKeyset * definition, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
