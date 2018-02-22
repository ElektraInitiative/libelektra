/**
 * @file
 *
 * @brief Header for yamlsmith plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YAMLSMITH_H
#define ELEKTRA_PLUGIN_YAMLSMITH_H

#include <kdbplugin.h>

int elektraYamlsmithSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (yamlsmith);

#endif
