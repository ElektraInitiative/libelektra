/**
 * @file
 *
 * @brief Header for quickdump plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_QUICKDUMP_H
#define ELEKTRA_PLUGIN_QUICKDUMP_H

#include <elektra/kdbplugin.h>


int elektraQuickdumpGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraQuickdumpSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
