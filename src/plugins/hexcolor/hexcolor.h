/**
 * @file
 *
 * @brief Header for hexcolor plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_HEXCOLOR_H
#define ELEKTRA_PLUGIN_HEXCOLOR_H

#include <kdbplugin.h>


int elektraHexcolorGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHexcolorSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
