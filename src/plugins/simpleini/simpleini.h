/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_SIMPLEINI_H
#define ELEKTRA_PLUGIN_SIMPLEINI_H

#include <elektra/kdbplugin.h>


int elektraSimpleiniOpen (Plugin * handle, Key * errorKey);
int elektraSimpleiniClose (Plugin * handle, Key * errorKey);
int elektraSimpleiniGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSimpleiniSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSimpleiniError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
