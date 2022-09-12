/**
 * @file
 *
 * @brief Header for blockresolver plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BLOCKRESOLVER_H
#define ELEKTRA_PLUGIN_BLOCKRESOLVER_H

#include <kdbplugin.h>


int elektraBlockresolverClose (Plugin * handle, ElektraKey * errorKey);
int elektraBlockresolverGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraBlockresolverSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraBlockresolverError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraBlockresolverCommit (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
