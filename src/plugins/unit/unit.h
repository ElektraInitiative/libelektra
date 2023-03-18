/**
 * @file
 *
 * @brief Header for unit plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_UNIT_H
#define ELEKTRA_PLUGIN_UNIT_H

#include <elektra/plugin/plugin.h>


int elektraUnitGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraUnitSet (Plugin * handle, KeySet * ks, Key * parentKey);


Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
