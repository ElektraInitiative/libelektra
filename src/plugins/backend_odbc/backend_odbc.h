/**
 * @file
 *
 * @brief Header file for the ODBC backend plugin
 *
 * This file contains the functions that are called by the Elektra core when it uses plugins
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_BACKEND_ODBC_H
#define ELEKTRA_BACKEND_ODBC_H

#include <kdbplugin.h>

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * handle, KeySet * definition, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * plugin, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (error) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, Key * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif // ELEKTRA_BACKEND_ODBC_H
