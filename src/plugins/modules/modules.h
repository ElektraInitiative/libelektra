/**
 * @file
 *
 * @brief Header for modules plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MODULES_H
#define ELEKTRA_PLUGIN_MODULES_H

#include <kdbplugin.h>

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
