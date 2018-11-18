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

int ELEKTRA_PLUGIN_FUNCTION (mmapstorage, open) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (mmapstorage, close) (Plugin * handle, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (mmapstorage, get) (Plugin * handle, KeySet * ks, Key * parentKey);
int ELEKTRA_PLUGIN_FUNCTION (mmapstorage, set) (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (mmapstorage);

#endif
