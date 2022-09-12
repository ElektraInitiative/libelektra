/**
 * @file
 *
 * @brief Header for gitresolver plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_GITRESOLVER_H
#define ELEKTRA_PLUGIN_GITRESOLVER_H

#include <kdbplugin.h>


int elektraGitresolverOpen (Plugin * handle, ElektraKey * errorKey);
int elektraGitresolverClose (Plugin * handle, ElektraKey * errorKey);
int elektraGitresolverGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraGitresolverSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraGitresolverError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraGitresolverCommit (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
