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

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>


int elektraBlockresolverClose (Plugin * handle, Key * errorKey);
int elektraBlockresolverGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBlockresolverSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBlockresolverError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBlockresolverCommit (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
