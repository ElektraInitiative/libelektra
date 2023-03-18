/**
 * @file
 *
 * @brief Header for mozprefs plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_PREFS_H
#define ELEKTRA_PLUGIN_PREFS_H

#include <elektra/plugin/plugin.h>


int elektraMozprefsGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMozprefsSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
