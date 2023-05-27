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

int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle, KeySet * definition, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif // ELEKTRA_BACKEND_H
