/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_LINE_H
#define ELEKTRA_PLUGIN_LINE_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>


int elektraLineGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLineSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
