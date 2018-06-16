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

#include <kdbplugin.h>
#include <kdbmmap.h>


typedef struct _dynArray DynArray;

struct _dynArray
{
	size_t size;
	size_t alloc;
	Key ** keyArray;
	Key ** mappedKeyArray;
};

#ifdef DEBUG
int findOrInsert (Key * key, DynArray * dynArray);
#endif


int elektraMmapstorageOpen (Plugin * handle, Key * errorKey);
int elektraMmapstorageClose (Plugin * handle, Key * errorKey);
int elektraMmapstorageGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMmapstorageSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (mmapstorage);

#endif
