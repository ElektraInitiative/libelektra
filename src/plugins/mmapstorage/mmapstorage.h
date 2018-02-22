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

#define ELEKTRA_MAGIC_MMAP_NUMBER (0x0A6172746B656C45)

typedef struct _mmapHeader MmapHeader;

struct _mmapHeader
{
	size_t mmapMagicNumber; /**<Magic number for consistency check */
	size_t mmapSize;	/**<Size of the complete mapping in bytes */
	size_t numKeySets;      /**<Number of KeySets inlcuding meta KS */
	size_t ksAlloc;		/**<Sum of all KeySet->alloc sizes */
	size_t numKeys;		/**<Number of Keys including meta Keys */
	size_t dataSize;	/**<Size of the data block in bytes: dynamic properties like key name, value, etc. */
	char * mmapAddr;	/**<Base pointer to mapped region (points to the start of this struct) */
};

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
