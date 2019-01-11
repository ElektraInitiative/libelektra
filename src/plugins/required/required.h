/**
 * @file
 *
 * @brief Header for required plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_REQUIRED_H
#define ELEKTRA_PLUGIN_REQUIRED_H

#include <kdbplugin.h>


int elektraRequiredGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraRequiredSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (required);

#endif
