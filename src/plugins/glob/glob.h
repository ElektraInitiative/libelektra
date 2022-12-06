/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_GLOB_H
#define ELEKTRA_PLUGIN_GLOB_H

#include <elektra/kdbplugin.h>

#include <stdlib.h>
#include <string.h>

int elektraGlobOpen (Plugin * handle, Key * errorKey);
int elektraGlobClose (Plugin * handle, Key * errorKey);
int elektraGlobGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGlobSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGlobError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
