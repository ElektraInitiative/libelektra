/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_KDB_FUNCTIONS_H
#define ELEKTRA_PLUGIN_CRYPTO_KDB_FUNCTIONS_H

#include <kdbplugin.h>

// kdb functions
int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, ElektraKey * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, ElektraKey * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (checkconf) (ElektraKey * errorKey, ElektraKeyset * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
