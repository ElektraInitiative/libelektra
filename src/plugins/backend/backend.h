/**
 * @file
 *
 * @brief Header file for the backend plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_BACKEND_H
#define ELEKTRA_BACKEND_H

#include <kdbplugin.h>

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, ElektraKey * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle, ElektraKeyset * definition, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (error) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, ElektraKey * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif // ELEKTRA_BACKEND_H
