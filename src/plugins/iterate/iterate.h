/**
 * @file
 *
 * @brief Header for iterate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_ITERATE_H
#define ELEKTRA_PLUGIN_ITERATE_H

#include <kdbplugin.h>


int elektraIterateOpen (Plugin * handle, ElektraKey * errorKey);
int elektraIterateClose (Plugin * handle, ElektraKey * errorKey);
int elektraIterateGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraIterateSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraIterateError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
