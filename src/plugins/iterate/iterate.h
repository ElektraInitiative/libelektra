/**
 * @file
 *
 * @brief Header for iterate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_ITERATE_H
#define ELEKTRA_PLUGIN_ITERATE_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>


int elektraIterateOpen (Plugin * handle, Key * errorKey);
int elektraIterateClose (Plugin * handle, Key * errorKey);
int elektraIterateGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraIterateSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraIterateError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
