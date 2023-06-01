/**
 * @file
 *
 * @brief filter plugin for the Base64 encoding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BASE64_H
#define ELEKTRA_PLUGIN_BASE64_H

#include "./base64_functions.h"

// kdb functions
int PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);
int PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
