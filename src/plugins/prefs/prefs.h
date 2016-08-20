/**
 * @file
 *
 * @brief Header for prefs plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_PREFS_H
#define ELEKTRA_PLUGIN_PREFS_H

#include <kdbplugin.h>


int elektraPrefsGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraPrefsSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (prefs);

#endif
