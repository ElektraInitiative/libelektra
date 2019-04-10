/**
 * @file
 *
 * @brief Header file for the units plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_UNITS_H
#define ELEKTRA_PLUGIN_UNITS_H

#include <kdbplugin.h>


int elektraUnitsGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraUnitsSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
