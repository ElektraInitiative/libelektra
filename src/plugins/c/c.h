/**
 * @file
 *
 * @brief Header for c plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_C_H
#define ELEKTRA_PLUGIN_C_H

#include <kdbplugin.h>

int elektraCGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
