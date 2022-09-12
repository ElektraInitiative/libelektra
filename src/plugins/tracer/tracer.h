/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbplugin.h>

#include <stdio.h>

int elektraTracerOpen (Plugin * handle, ElektraKey * errorKey);
int elektraTracerClose (Plugin * handle, ElektraKey * errorKey);
int elektraTracerGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraTracerSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraTracerError (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;
