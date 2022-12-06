/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_FCRYPT_H
#define ELEKTRA_PLUGIN_FCRYPT_H

#include <elektra/kdbplugin.h>

// kdb functions
int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (checkconf) (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#define ELEKTRA_FCRYPT_DEFAULT_TMPDIR "/tmp"

#define ELEKTRA_FCRYPT_CONFIG_TEXTMODE "/fcrypt/textmode"
#define ELEKTRA_FCRYPT_CONFIG_TMPDIR "/fcrypt/tmpdir"

#endif
