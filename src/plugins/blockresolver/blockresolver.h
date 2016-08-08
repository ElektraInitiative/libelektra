/**
 * @file
 *
 * @brief Header for blockresolver plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BLOCKRESOLVER_H
#define ELEKTRA_PLUGIN_BLOCKRESOLVER_H

#include <kdbplugin.h>


int elektraBlockresolverOpen (Plugin * handle, Key * errorKey);
int elektraBlockresolverClose (Plugin * handle, Key * errorKey);
int elektraBlockresolverGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBlockresolverSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBlockresolverError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraBlockresolverCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (blockresolver);

#endif
