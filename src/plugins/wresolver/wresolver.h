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


int elektraWresolverOpen (Plugin * handle, Key * errorKey);
int elektraWresolverClose (Plugin * handle, Key * errorKey);
int elektraWresolverGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraWresolverSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraWresolverError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (wresolver);

#endif
