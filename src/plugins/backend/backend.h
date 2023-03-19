/**
 * @file
 *
 * @brief Header file for the backend plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_BACKEND_H
#define ELEKTRA_BACKEND_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle, KeySet * definition, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (error) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, Key * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif // ELEKTRA_BACKEND_H
