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

int elektraVersionGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraVersionSet (Plugin * handle, KeySet * ks, Key * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
