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

#include <elektra/plugin/plugin.h>


int elektraDesktopGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDesktopSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
