/**
 * @file
 *
 * @brief Header for enum plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_ENUM_H
#define ELEKTRA_PLUGIN_ENUM_H

#include <kdbplugin.h>


int elektraEnumGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraEnumSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
