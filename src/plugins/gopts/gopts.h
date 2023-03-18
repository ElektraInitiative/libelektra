/**
 * @file
 *
 * @brief Header for opts plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_OPTS_H
#define ELEKTRA_PLUGIN_OPTS_H

#include <elektra/plugin/plugin.h>

int elektraGOptsGet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
