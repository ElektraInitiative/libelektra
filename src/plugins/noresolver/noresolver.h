/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NORESOLVER_H
#define ELEKTRA_PLUGIN_NORESOLVER_H

#include <kdbplugin.h>

#define ELEKTRA_PLUGIN_NAME "noresolver"


int elektraNoresolverOpen (Plugin * handle, Key * errorKey);
int elektraNoresolverClose (Plugin * handle, Key * errorKey);
int elektraNoresolverGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNoresolverSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNoresolverError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (noresolver);

#endif
