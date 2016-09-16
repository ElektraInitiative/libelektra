/**
 * @file
 *
 * @brief Header for desktop plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_DESKTOP_H
#define ELEKTRA_PLUGIN_DESKTOP_H

#include <kdbplugin.h>


int elektraDesktopGet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (desktop);

#endif
