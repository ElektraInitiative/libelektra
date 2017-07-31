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

#define ELEKTRA_MAGIC_MMAP_NUMBER (0x656c656b747261)

typedef struct _mmapHeader MmapHeader;

struct _mmapHeader
{
	size_t mmapMagicNumber;
	size_t mmapSize;
	size_t numKeys;
	size_t numMetaKeys;
	size_t numMetaKeySets;
	char * addr;
};

typedef struct _dynArray DynArray;

struct _dynArray
{
	size_t size;
	size_t alloc;
	size_t * keyArray;
	
};


int elektraMmapstorageOpen (Plugin * handle, Key * errorKey);
int elektraMmapstorageClose (Plugin * handle, Key * errorKey);
int elektraMmapstorageGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMmapstorageSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMmapstorageError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMmapstorageCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (mmapstorage);

#endif
