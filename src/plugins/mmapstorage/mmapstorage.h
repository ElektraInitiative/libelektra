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

#include <kdbplugin.h>

int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, ElektraKey * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, ElektraKey * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
