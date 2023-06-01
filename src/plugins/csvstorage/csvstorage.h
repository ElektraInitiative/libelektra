/**
 * @file
 *
 * @brief Header for csvstorage plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CSVSTORAGE_H
#define ELEKTRA_PLUGIN_CSVSTORAGE_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>


int elektraCsvstorageGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCsvstorageSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
