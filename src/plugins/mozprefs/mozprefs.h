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

#include <kdbplugin.h>


int elektraMozprefsGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraMozprefsSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
