/**
 * @file
 *
 * @brief Header for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_DIRECTORYVALUE_H
#define ELEKTRA_PLUGIN_DIRECTORYVALUE_H

#include <kdbplugin.h>
int elektraDirectoryvalueGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDirectoryvalueSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (directoryvalue);

#endif
