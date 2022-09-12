/**
 * @file
 *
 * @brief Header for desktop plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_DESKTOP_H
#define ELEKTRA_PLUGIN_DESKTOP_H

#include <kdbplugin.h>


int elektraDesktopGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraDesktopSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
