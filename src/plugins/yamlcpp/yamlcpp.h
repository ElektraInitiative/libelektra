/**
 * @file
 *
 * @brief Header for yamlcpp plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YAMLCPP_H
#define ELEKTRA_PLUGIN_YAMLCPP_H

#include <kdbplugin.h>

int elektraYamlcppGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraYamlcppSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (yamlcpp);

#endif
