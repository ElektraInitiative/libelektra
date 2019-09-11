/**
 * @file
 *
 * @brief Header for spec plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SPEC_H
#define ELEKTRA_PLUGIN_SPEC_H

#include <kdbplugin.h>


int elektraSpecGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSpecSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
