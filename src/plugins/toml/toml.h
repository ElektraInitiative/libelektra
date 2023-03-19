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

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

int elektraTomlGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraTomlSet (Plugin * handle, KeySet * returned, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif // ELEKTRA_PLUGIN_TOML_H
