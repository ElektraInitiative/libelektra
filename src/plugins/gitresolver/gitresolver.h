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

#include <elektra/plugin/plugin.h>


int elektraGitresolverOpen (Plugin * handle, Key * errorKey);
int elektraGitresolverClose (Plugin * handle, Key * errorKey);
int elektraGitresolverGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGitresolverSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGitresolverError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGitresolverCommit (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
