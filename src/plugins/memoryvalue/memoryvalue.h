/**
 * @file
 *
 * @brief Header for memorycheck plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MEMORYVALUE_H
#define ELEKTRA_PLUGIN_MEMORYVALUE_H

#include <kdbplugin.h>


int elektraMemoryvalueGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMemoryvalueSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
