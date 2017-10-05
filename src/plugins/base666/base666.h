/**
 * @file
 *
 * @brief Filter plugin for Base64 encoding
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BASE666_H
#define ELEKTRA_PLUGIN_BASE666_H

#include <kdbplugin.h>
#include <kdbtypes.h>
#include <stdio.h>

#include "../base64/base64_functions.h"

// kdb functions
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, set) (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (base666);

#endif
