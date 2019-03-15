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


int elektraHexcolorOpen (Plugin * handle, Key * errorKey);
int elektraHexcolorClose (Plugin * handle, Key * errorKey);
int elektraHexcolorGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHexcolorSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHexcolorError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHexcolorCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
