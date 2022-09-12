/**
 * @file
 *
 * @brief Header for process plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_PROCESS_H
#define ELEKTRA_PLUGIN_PROCESS_H

#include <kdbplugin.h>


int elektraProcessOpen (Plugin * handle, ElektraKey * errorKey);
int elektraProcessClose (Plugin * handle, ElektraKey * errorKey);
int elektraProcessGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraProcessSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraProcessError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraProcessCheckConf (ElektraKey * errorKey, ElektraKeyset * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;


#endif
