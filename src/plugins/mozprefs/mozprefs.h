/**
 * @file
 *
 * @brief Header for mozprefs plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_PREFS_H
#define ELEKTRA_PLUGIN_PREFS_H

#include <kdbplugin.h>


int elektraMozprefsGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMozprefsSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (mozprefs);

#endif
