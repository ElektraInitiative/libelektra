/**
 * @file
 *
 * @brief Header for rgbcolor plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_RGBCOLOR_H
#define ELEKTRA_PLUGIN_RGBCOLOR_H

#include <elektra/plugin/plugin.h>


int elektraRgbcolorGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRgbcolorSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
