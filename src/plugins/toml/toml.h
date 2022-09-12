/**
 * @file
 *
 * @brief Header for the toml plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TOML_H
#define ELEKTRA_PLUGIN_TOML_H

#include <kdbplugin.h>

int elektraTomlGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey);
int elektraTomlSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif // ELEKTRA_PLUGIN_TOML_H
