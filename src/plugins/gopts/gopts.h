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

#include <elektra/kdbplugin.h>

int elektraGOptsGet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
