/**
 * @file
 *
 * @brief Header for multifile plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MULTIFILE_H
#define ELEKTRA_PLUGIN_MULTIFILE_H

#include <kdbplugin.h>


int elektraMultifileOpen (Plugin * handle, ElektraKey * errorKey);
int elektraMultifileClose (Plugin * handle, ElektraKey * errorKey);
int elektraMultifileGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraMultifileSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraMultifileError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraMultifileCommit (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraMultifileCheckConf (ElektraKey * errorKey, ElektraKeyset * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
