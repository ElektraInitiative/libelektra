/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_WRESOLVER_H
#define ELEKTRA_PLUGIN_WRESOLVER_H

#include <kdbplugin.h>


int elektraWresolverOpen (Plugin * handle, ElektraKey * errorKey);
int elektraWresolverClose (Plugin * handle, ElektraKey * errorKey);
int elektraWresolverGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraWresolverSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraWresolverCommit (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraWresolverError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
