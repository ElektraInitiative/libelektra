/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_GLOB_H
#define ELEKTRA_PLUGIN_GLOB_H

#include <kdbplugin.h>

#include <stdlib.h>
#include <string.h>

int elektraGlobOpen (Plugin * handle, ElektraKey * errorKey);
int elektraGlobClose (Plugin * handle, ElektraKey * errorKey);
int elektraGlobGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraGlobSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraGlobError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
