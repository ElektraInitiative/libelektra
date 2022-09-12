/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NORESOLVER_H
#define ELEKTRA_PLUGIN_NORESOLVER_H

#include <kdbplugin.h>

#define ELEKTRA_PLUGIN_NAME "noresolver"


int elektraNoresolverGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraNoresolverSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraNoresolverError (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraNoresolverCommit (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
