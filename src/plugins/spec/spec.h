/**
 * @file
 *
 * @brief Header for spec plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SPEC_H
#define ELEKTRA_PLUGIN_SPEC_H

#include <kdbplugin.h>


int elektraSpecGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSpecSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSpecClose (Plugin * handle, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (spec);

#endif
