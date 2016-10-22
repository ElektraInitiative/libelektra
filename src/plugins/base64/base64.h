/**
 * @file
 *
 * @brief filter plugin for the Base64 encoding
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BASE64_H
#define ELEKTRA_PLUGIN_BASE64_H

#include <kdbplugin.h>
#include <kdbtypes.h>
#include <stdio.h>

#include "base64_functions.h"

// kdb functions
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, set) (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (base64);

#endif
