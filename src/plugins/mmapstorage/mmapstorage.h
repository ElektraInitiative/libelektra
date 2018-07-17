/**
 * @file
 *
 * @brief Header for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MMAPSTORAGE_H
#define ELEKTRA_PLUGIN_MMAPSTORAGE_H

#include <kdbmmap.h>
#include <kdbplugin.h>


int elektraMmapstorageOpen (Plugin * handle, Key * errorKey);
int elektraMmapstorageClose (Plugin * handle, Key * errorKey);
int elektraMmapstorageGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMmapstorageSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (mmapstorage);

#endif
