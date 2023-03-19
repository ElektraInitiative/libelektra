/**
 * @file
 *
 * @brief Header for mmapstorage plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MMAPSTORAGE_H
#define ELEKTRA_PLUGIN_MMAPSTORAGE_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
