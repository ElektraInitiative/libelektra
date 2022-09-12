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

#include <kdbplugin.h>


int elektraUnitGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraUnitSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);


Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
