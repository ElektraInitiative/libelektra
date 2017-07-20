/**
 * @file
 *
 * @brief Header for yaml plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YAML_H
#define ELEKTRA_PLUGIN_YAML_H

#include <kdbplugin.h>

int elektraYamlGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraYamlSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (yaml);

#endif
