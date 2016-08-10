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


int elektraDesktopOpen (Plugin * handle, Key * errorKey);
int elektraDesktopClose (Plugin * handle, Key * errorKey);
int elektraDesktopGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDesktopSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDesktopError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDesktopCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (desktop);

#endif
