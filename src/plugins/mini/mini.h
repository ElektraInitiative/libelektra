/**
 * @file
 *
 * @brief Header for mini plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MINI_H
#define ELEKTRA_PLUGIN_MINI_H

#include <elektra/plugin/plugin.h>


int elektraMiniGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMiniSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
