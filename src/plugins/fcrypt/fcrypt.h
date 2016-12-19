/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_FCRYPT_H
#define ELEKTRA_PLUGIN_FCRYPT_H

#include <kdbplugin.h>

// kdb functions
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, set) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, checkconf) (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (crypto);

#endif
