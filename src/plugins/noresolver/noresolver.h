/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_NORESOLVER_H
#define ELEKTRA_PLUGIN_NORESOLVER_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

#define ELEKTRA_PLUGIN_NAME "noresolver"


int elektraNoresolverGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNoresolverSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNoresolverError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNoresolverCommit (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
