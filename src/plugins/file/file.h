/**
 * @file
 *
 * @brief Header for file plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_FILE_H
#define ELEKTRA_PLUGIN_FILE_H

#include <kdbplugin.h>


int elektraFileGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraFileSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (file);

#endif
