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

#include <kdbplugin.h>


int elektraCsvstorageGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraCsvstorageSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
