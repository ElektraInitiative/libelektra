/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_SIMPLEINI_H
#define ELEKTRA_PLUGIN_SIMPLEINI_H

#include <kdbplugin.h>


int elektraSimpleiniOpen (Plugin * handle, ElektraKey * errorKey);
int elektraSimpleiniClose (Plugin * handle, ElektraKey * errorKey);
int elektraSimpleiniGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraSimpleiniSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraSimpleiniError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
