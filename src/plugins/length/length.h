/**
 * @file
 *
 * @brief Header for len plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LENGTH_H
#define ELEKTRA_PLUGIN_LENGTH_H

#include <elektra/plugin/plugin.h>

int elektraLengthGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLengthSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
