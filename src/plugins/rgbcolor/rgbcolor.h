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

#include <kdbplugin.h>


int elektraRgbcolorGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraRgbcolorSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
