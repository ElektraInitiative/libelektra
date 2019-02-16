/**
 * @file
 *
 * @brief Header for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SPECLOAD_H
#define ELEKTRA_PLUGIN_SPECLOAD_H

#include <kdbplugin.h>


int elektraSpecloadOpen (Plugin * handle, Key * errorKey);
int elektraSpecloadClose (Plugin * handle, Key * errorKey);
int elektraSpecloadGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSpecloadCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (specload);

#endif
