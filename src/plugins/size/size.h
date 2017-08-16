/**
 * @file
 *
 * @brief Header for size plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SIZE_H
#define ELEKTRA_PLUGIN_SIZE_H

#include <kdbplugin.h>

int elektraSizeClose (Plugin * handle, Key * parentKey);
int elektraSizeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSizeSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSizeCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (size);

#endif
