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

#include "../base64/base64_functions.h"

int elektraBase666Get (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBase666Set (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (base666);

#endif
