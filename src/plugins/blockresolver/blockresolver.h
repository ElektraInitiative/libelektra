/**
 * @file
 *
 * @brief Header for blockresolver plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BLOCKRESOLVER_H
#define ELEKTRA_PLUGIN_BLOCKRESOLVER_H

#include <kdbplugin.h>


int elektraBlockresolverClose (Plugin * handle, Key * errorKey);
int elektraBlockresolverGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBlockresolverSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBlockresolverError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (blockresolver);

#endif
