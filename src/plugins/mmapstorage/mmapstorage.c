/**
 * @file
 *
 * @brief Source for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */
#define _XOPEN_SOURCE 600

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "mmapstorage.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbmmap.h>
#include <kdbprivate.h>

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <assert.h>
#include <errno.h>
#include <limits.h>    // SSIZE_MAX
#include <stdint.h>    // uintN_t, uintptr_t
#include <stdio.h>     // fopen(), fileno(), fread()
#include <stdlib.h>    // strtol()
#include <string.h>    // memcmp()
#include <sys/mman.h>  // mmap()
#include <sys/stat.h>  // stat()
#include <sys/types.h> // ftruncate ()
#include <unistd.h>    // close(), ftruncate()

#ifdef ENABLE_MMAP_CHECKSUM
#include <zlib.h> // crc32()
#endif

/* -- Macros & Definitions -------------------------------------------------------------------------------------------------------------- */

#define SIZEOF_KEY (sizeof (Key))
#define SIZEOF_KEY_PTR (sizeof (Key *))
#define SIZEOF_KEYSET (sizeof (KeySet))
#define SIZEOF_KEYSET_PTR (sizeof (KeySet *))
#define SIZEOF_MMAPHEADER (sizeof (MmapHeader))
#define SIZEOF_MMAPMETADATA (sizeof (MmapMetaData))
#define SIZEOF_MMAPFOOTER (sizeof (MmapFooter))
#define SIZEOF_ADDR_STRING (19) // format: 0xADDR -> ADDR in hex, for 64bit addr: 2 bytes (0x) + 16 bytes (ADDR) + 1 byte (ending null)

struct _dynArray
{
	size_t size;
	size_t alloc;
	Key ** keyArray;
	Key ** mappedKeyArray;
};

typedef struct _dynArray DynArray;

struct _mmapAddr
{
	KeySet * const ksPtr;
	char * metaKsPtr;
	char * ksArrayPtr;
	char * metaKsArrayPtr;
	char * keyPtr;
	char * dataPtr;

	const uintptr_t mmapAddrInt;
};

typedef struct _mmapAddr MmapAddr;

static KeySet magicKeySet;
static Key magicKey;


/* -- File handling --------------------------------------------------------------------------------------------------------------------- */

static FILE * openFile (Key * parentKey, const char * mode)
{
	FILE * fp;
	ELEKTRA_LOG_DEBUG ("opening file %s", keyString (parentKey));

	if ((fp = fopen (keyString (parentKey), mode)) == 0)
	{
		ELEKTRA_LOG_WARNING ("error opening file %s", keyString (parentKey));
	}
	return fp;
}

static int truncateFile (FILE * fp, size_t mmapsize, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG_DEBUG ("truncating file %s", keyString (parentKey));

	int fd = fileno (fp);
	if ((ftruncate (fd, mmapsize)) == -1)
	{
		ELEKTRA_LOG_WARNING ("error truncating file %s", keyString (parentKey));
		return -1;
	}
	return 1;
}

static int statFile (struct stat * sbuf, Key * parentKey)
{
	ELEKTRA_LOG_DEBUG ("stat() on file %s", keyString (parentKey));

	if (stat (keyString (parentKey), sbuf) == -1)
	{
		ELEKTRA_LOG_WARNING ("error on stat() for file %s", keyString (parentKey));
		return -1;
	}
	return 1;
}

static char * mmapFile (void * addr, FILE * fp, size_t mmapSize, int mapOpts, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG_DEBUG ("mapping file %s", keyString (parentKey));

	int fd = fileno (fp);
	char * mappedRegion = mmap (addr, mmapSize, PROT_READ | PROT_WRITE, mapOpts, fd, 0);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("error mapping file %s\nmmapSize: %zu", keyString (parentKey), mmapSize);
		return MAP_FAILED;
	}
	return mappedRegion;
}

/* -- DynArray -------------------------------------------------------------------------------------------------------------------------- */

static DynArray * newDynArray (void)
{
	DynArray * dynArray = elektraCalloc (sizeof (DynArray));
	dynArray->keyArray = elektraCalloc (sizeof (Key *) * 8);
	dynArray->mappedKeyArray = 0;
	dynArray->size = 0;
	dynArray->alloc = 8;
	return dynArray;
}

static void delDynArray (DynArray * dynArray)
{
	if (!dynArray)
	{
		return;
	}
	if (dynArray->keyArray)
	{
		elektraFree (dynArray->keyArray);
	}
	if (dynArray->mappedKeyArray)
	{
		elektraFree (dynArray->mappedKeyArray);
	}
	elektraFree (dynArray);
}

/*
#ifdef DEBUG
int findOrInsert (Key * key, DynArray * dynArray)
#else
static int findOrInsert (Key * key, DynArray * dynArray)
#endif
*/
static int findOrInsert (Key * key, DynArray * dynArray)
{
	size_t l = 0;
	size_t h = dynArray->size;
	size_t m;
	ELEKTRA_LOG_DEBUG ("l: %zu", l);
	ELEKTRA_LOG_DEBUG ("h: %zu", h);
	ELEKTRA_LOG_DEBUG ("dynArray->size: %zu", dynArray->size);
	ELEKTRA_LOG_DEBUG ("dynArray->alloc: %zu", dynArray->alloc);

	while (l < h)
	{
		m = (l + h) >> 1;
		ELEKTRA_LOG_DEBUG ("m: %zu", m);

		if (dynArray->keyArray[m] > key)
		{
			h = m;
		}
		else if (dynArray->keyArray[m] < key)
		{
			l = ++m;
		}
		else
		{
			return 1; // found
		}
	}
	// insert key at index l
	if (dynArray->size == dynArray->alloc)
	{
		// doubling the array size to keep reallocations logarithmic
		size_t oldAllocSize = dynArray->alloc;
		if (oldAllocSize > (SIZE_MAX / 2))
		{
			return -1; // error
		}
		Key ** new = elektraCalloc ((2 * oldAllocSize) * sizeof (Key *));
		memcpy (new, dynArray->keyArray, dynArray->size * sizeof (Key *));
		elektraFree (dynArray->keyArray);
		dynArray->keyArray = new;
		dynArray->alloc = 2 * oldAllocSize;
	}

	memmove ((void *) (dynArray->keyArray + l + 1), (void *) (dynArray->keyArray + l), ((dynArray->size) - l) * (sizeof (size_t)));
	dynArray->keyArray[l] = key;
	dynArray->size += 1;

	return 0; // inserted
}

static ssize_t find (Key * key, DynArray * dynArray)
{
	size_t l = 0;
	size_t h = dynArray->size;
	size_t m;
	ELEKTRA_LOG_DEBUG ("l: %zu", l);
	ELEKTRA_LOG_DEBUG ("h: %zu", h);
	ELEKTRA_LOG_DEBUG ("dynArray->size: %zu", dynArray->size);
	ELEKTRA_LOG_DEBUG ("dynArray->alloc: %zu", dynArray->alloc);

	while (l < h)
	{
		m = (l + h) >> 1;
		ELEKTRA_LOG_DEBUG ("m: %zu", m);

		if (dynArray->keyArray[m] > key)
		{
			h = m;
		}
		else if (dynArray->keyArray[m] < key)
		{
			l = ++m;
		}
		else
		{
			if (m < SSIZE_MAX)
			{
				return m; // found
			}
			else
			{
				return -1;
			}
		}
	}

	return -1;
}

/* -- Internal Functions  --------------------------------------------------------------------------------------------------------------- */

static void initHeader (MmapHeader * mmapHeader)
{
	memset (mmapHeader, 0, SIZEOF_MMAPHEADER);
	mmapHeader->mmapMagicNumber = ELEKTRA_MAGIC_MMAP_NUMBER;
}

static void initMetaData (MmapMetaData * mmapMetaData)
{
	memset (mmapMetaData, 0, SIZEOF_MMAPMETADATA);
}

static void initFooter (MmapFooter * mmapFooter)
{
	mmapFooter->mmapMagicNumber = ELEKTRA_MAGIC_MMAP_NUMBER;
}

static int readHeader (FILE * fp, MmapHeader * mmapHeader, MmapMetaData * mmapMetaData)
{
	memset (mmapHeader, 0, SIZEOF_MMAPHEADER);
	memset (mmapMetaData, 0, SIZEOF_MMAPMETADATA);

	if (fread (mmapHeader, SIZEOF_MMAPHEADER, 1, fp) != 1 || fread (mmapMetaData, SIZEOF_MMAPMETADATA, 1, fp) != 1)
	{
		return -1;
	}

	if (mmapHeader->mmapMagicNumber == ELEKTRA_MAGIC_MMAP_NUMBER)
	{
		return 0;
	}

	return -1;
}

static int readFooter (char * mappedRegion, MmapHeader * mmapHeader)
{
	MmapFooter * mmapFooter = (MmapFooter *) (mappedRegion + mmapHeader->allocSize - SIZEOF_MMAPFOOTER);

	if (mmapFooter->mmapMagicNumber == ELEKTRA_MAGIC_MMAP_NUMBER)
	{
		return 0;
	}

	return -1;
}

static void writeMagicData (char * dest)
{
	KeySet * destKeySet = (KeySet *) (dest + SIZEOF_MMAPHEADER + SIZEOF_MMAPMETADATA);
	Key * keyPtr = (Key *) ((char *) destKeySet + SIZEOF_KEYSET);
	memcpy (destKeySet, &magicKeySet, SIZEOF_KEYSET);
	memcpy (keyPtr, &magicKey, SIZEOF_KEY);
}

/* -- Verification Functions  ----------------------------------------------------------------------------------------------------------- */

/**
 * @brief Generate magic number depending on pointer size.
 *
 * Generates magic number to detect arbitrary byte swaps.
 * For each byte of the number, a different value is assigned.
 * E.g.: Systems with 8-byte pointers will have the magic number 0x0706050403020100.
 *
 * @return generated magic number
 */
static uintptr_t generateMagicNumber (void)
{
	uintptr_t ret = 0;

	size_t ptrBytes = sizeof (void *);
	for (uintptr_t i = 0; i < ptrBytes; ++i)
	{
		ret |= (i << (i * 8));
	}

	return ret;
}

static void initMagicKeySet (const uintptr_t magicNumber)
{
	magicKeySet.array = (Key **) magicNumber;
	magicKeySet.size = SIZE_MAX;
	magicKeySet.alloc = 0;
	magicKeySet.cursor = (Key *) ~magicNumber;
	magicKeySet.current = SIZE_MAX / 2;
	magicKeySet.flags = KS_FLAG_MMAP_ARRAY | KS_FLAG_SYNC;
	magicKeySet.mmapMetaData = (MmapMetaData *) 0xE1EF;
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	magicKeySet.opmphm = (Opmphm *) 0xFFFE;
#endif
}

static void initMagicKey (const uintptr_t magicNumber)
{
	magicKey.data.v = (void *) ~magicNumber;
	magicKey.dataSize = SIZE_MAX;
	magicKey.key = (char *) magicNumber;
	magicKey.keySize = UINT16_MAX;
	magicKey.keyUSize = 42;
	magicKey.flags = KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_DATA | KEY_FLAG_MMAP_KEY | KEY_FLAG_SYNC;
	magicKey.ksReference = SIZE_MAX / 2;
	magicKey.meta = (KeySet *) 0xFFFE;
}

static int verifyMagicKeySet (KeySet * ks)
{
	if (!ks)
	{
		return -1;
	}

	return memcmp (ks, &magicKeySet, SIZEOF_KEYSET);
}

static int verifyMagicKey (Key * key)
{
	if (!key)
	{
		return -1;
	}

	return memcmp (key, &magicKey, SIZEOF_KEY);
}

static int verifyMagicData (char * mappedRegion)
{
	KeySet * destKeySet = (KeySet *) (mappedRegion + SIZEOF_MMAPHEADER + SIZEOF_MMAPMETADATA);
	Key * keyPtr = (Key *) ((char *) destKeySet + SIZEOF_KEYSET);

	if ((verifyMagicKey (keyPtr) != 0) || (verifyMagicKeySet (destKeySet) != 0))
	{
		return -1;
	}

	return 0;
}

#ifdef ENABLE_MMAP_CHECKSUM
static int verifyChecksum (char * mappedRegion, MmapHeader * mmapHeader)
{
	uint32_t checksum = crc32 (0L, Z_NULL, 0);
	checksum = crc32 (checksum, (const unsigned char *) (mappedRegion + SIZEOF_MMAPHEADER), mmapHeader->cksumSize);

	if (checksum != mmapHeader->checksum)
	{
		ELEKTRA_LOG_WARNING ("old checksum: %ul", mmapHeader->checksum);
		ELEKTRA_LOG_WARNING ("new checksum: %ul", checksum);
		return -1;
	}
	return 0;
}
#endif

static void calculateMmapDataSize (MmapHeader * mmapHeader, MmapMetaData * mmapMetaData, KeySet * returned, DynArray * dynArray)
{
	Key * cur;
	ksRewind (returned);
	size_t dataBlocksSize = 0; // keyName and keyValue
	size_t metaKeySets = 0;
	size_t metaKsAlloc = 0; // sum of allocation sizes for all meta-keysets
	while ((cur = ksNext (returned)) != 0)
	{
		dataBlocksSize += (cur->keySize + cur->keyUSize + cur->dataSize);

		if (cur->meta)
		{
			++metaKeySets;

			Key * curMeta;
			ksRewind (cur->meta);
			while ((curMeta = ksNext (cur->meta)) != 0)
			{
				if (findOrInsert (curMeta, dynArray) == 0)
				{
					// key was just inserted
					dataBlocksSize += (curMeta->keySize + curMeta->keyUSize + curMeta->dataSize);
				}
			}
			metaKsAlloc += (cur->meta->alloc);
		}
	}

	// TODO: add more docu about the size, magic KeySet, magic Key
	size_t keyArraySize = (returned->size + 1) * SIZEOF_KEY; // +1 for magic Key
	size_t keyPtrArraySize = (returned->alloc) * SIZEOF_KEY_PTR;

	// SIZEOF_KEYSET * 2 : magic KeySet + main KeySet
	size_t allocSize = (SIZEOF_KEYSET * 2) + keyPtrArraySize + keyArraySize + dataBlocksSize + (metaKeySets * SIZEOF_KEYSET) +
			   (metaKsAlloc * SIZEOF_KEY_PTR) + (dynArray->size * SIZEOF_KEY);
	mmapHeader->cksumSize = allocSize + SIZEOF_MMAPMETADATA; // cksumSize now contains size of all critical data

	size_t padding = sizeof (uint64_t) - (allocSize % sizeof (uint64_t)); // alignment for MMAP Footer at end of mapping
	allocSize += SIZEOF_MMAPHEADER + SIZEOF_MMAPMETADATA + SIZEOF_MMAPFOOTER + padding;

	mmapHeader->allocSize = allocSize;
	mmapMetaData->numKeySets = 1 + metaKeySets; // 1: main KeySet
	mmapMetaData->ksAlloc = returned->alloc + metaKsAlloc;
	mmapMetaData->numKeys = returned->size + dynArray->size;
}

static void writeMetaKeys (MmapAddr * mmapAddr, DynArray * dynArray)
{
	// allocate space in DynArray to remember the addresses of meta keys
	if (dynArray->size > 0)
	{
		dynArray->mappedKeyArray = elektraCalloc (dynArray->size * sizeof (Key *));
	}

	// write the meta keys into place
	Key * mmapMetaKey;
	Key * curMeta;
	void * metaKeyNamePtr;
	void * metaKeyValuePtr;
	for (size_t i = 0; i < dynArray->size; ++i)
	{
		curMeta = dynArray->keyArray[i];	// old key location
		mmapMetaKey = (Key *) mmapAddr->keyPtr; // new key location
		mmapAddr->keyPtr += SIZEOF_KEY;

		size_t keyNameSize = curMeta->keySize + curMeta->keyUSize;
		size_t keyValueSize = curMeta->dataSize;

		// move Key name
		if (curMeta->key)
		{
			memcpy (mmapAddr->dataPtr, curMeta->key, keyNameSize);
			metaKeyNamePtr = (mmapAddr->dataPtr - mmapAddr->mmapAddrInt);
			mmapAddr->dataPtr += keyNameSize;
		}
		else
		{
			metaKeyNamePtr = 0;
		}

		// move Key value
		if (curMeta->data.v)
		{
			memcpy (mmapAddr->dataPtr, curMeta->data.v, keyValueSize);
			metaKeyValuePtr = (mmapAddr->dataPtr - mmapAddr->mmapAddrInt);
			mmapAddr->dataPtr += keyValueSize;
		}
		else
		{
			metaKeyValuePtr = 0;
		}

		// move Key itself
		mmapMetaKey->flags = KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_KEY | KEY_FLAG_MMAP_DATA;
		mmapMetaKey->key = (char *) metaKeyNamePtr;
		mmapMetaKey->data.v = (void *) metaKeyValuePtr;
		mmapMetaKey->meta = 0;
		mmapMetaKey->ksReference = 0;
		mmapMetaKey->dataSize = curMeta->dataSize;
		mmapMetaKey->keySize = curMeta->keySize;
		mmapMetaKey->keyUSize = curMeta->keyUSize;

		dynArray->mappedKeyArray[i] = mmapMetaKey;
	}
}

static KeySet * writeMetaKeySet (Key * key, MmapAddr * mmapAddr, DynArray * dynArray)
{
	// write the meta KeySet
	KeySet * oldMeta = key->meta;
	KeySet * newMeta = 0;
	if (oldMeta)
	{
		newMeta = (KeySet *) mmapAddr->metaKsPtr;
		mmapAddr->metaKsPtr += SIZEOF_KEYSET;

		newMeta->flags = KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
		newMeta->array = (Key **) mmapAddr->metaKsArrayPtr;
		mmapAddr->metaKsArrayPtr += SIZEOF_KEY_PTR * oldMeta->alloc;

		keyRewindMeta (key);
		size_t metaKeyIndex = 0;
		Key * mappedMetaKey = 0;
		const Key * metaKey;
		while ((metaKey = keyNextMeta (key)) != 0)
		{
			// get address of mapped key and store it in the new array
			mappedMetaKey = dynArray->mappedKeyArray[find ((Key *) metaKey, dynArray)];
			newMeta->array[metaKeyIndex] = (Key *) ((char *) mappedMetaKey - mmapAddr->mmapAddrInt);
			if (mappedMetaKey->ksReference < SSIZE_MAX)
			{
				++(mappedMetaKey->ksReference);
			}
			++metaKeyIndex;
		}
		newMeta->array[oldMeta->size] = 0;
		newMeta->array = (Key **) ((char *) newMeta->array - mmapAddr->mmapAddrInt);
		newMeta->alloc = oldMeta->alloc;
		newMeta->size = oldMeta->size;
		newMeta->cursor = 0;
		newMeta->current = 0;
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
		newMeta->opmphm = 0;
#endif
		newMeta = (KeySet *) ((char *) newMeta - mmapAddr->mmapAddrInt);
	}

	return newMeta;
}

static void writeKeys (KeySet * keySet, MmapAddr * mmapAddr, DynArray * dynArray)
{
	Key * cur;
	Key * mmapKey;
	void * keyNamePtr;
	void * keyValuePtr;
	size_t keyIndex = 0;
	ksRewind (keySet);
	while ((cur = ksNext (keySet)) != 0)
	{
		mmapKey = (Key *) mmapAddr->keyPtr; // new key location
		mmapAddr->keyPtr += SIZEOF_KEY;
		size_t keyNameSize = cur->keySize + cur->keyUSize;
		size_t keyValueSize = cur->dataSize;

		// move Key name
		if (cur->key)
		{
			memcpy (mmapAddr->dataPtr, cur->key, keyNameSize);
			keyNamePtr = (mmapAddr->dataPtr - mmapAddr->mmapAddrInt);
			mmapAddr->dataPtr += keyNameSize;
		}
		else
		{
			keyNamePtr = 0;
		}


		// move Key value
		if (cur->data.v)
		{
			memcpy (mmapAddr->dataPtr, cur->data.v, keyValueSize);
			keyValuePtr = (mmapAddr->dataPtr - mmapAddr->mmapAddrInt);
			mmapAddr->dataPtr += keyValueSize;
		}
		else
		{
			keyValuePtr = 0;
		}

		// write the meta KeySet
		KeySet * newMeta = writeMetaKeySet (cur, mmapAddr, dynArray);

		// move Key itself
		mmapKey->flags = KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_KEY | KEY_FLAG_MMAP_DATA;
		mmapKey->key = (char *) keyNamePtr;
		mmapKey->keySize = cur->keySize;
		mmapKey->keyUSize = cur->keyUSize;
		mmapKey->data.v = (void *) keyValuePtr;
		mmapKey->dataSize = cur->dataSize;
		mmapKey->meta = newMeta;
		mmapKey->ksReference = 1;

		// write the relative Key pointer into the KeySet array
		((Key **) mmapAddr->ksArrayPtr)[keyIndex] = (Key *) ((char *) mmapKey - mmapAddr->mmapAddrInt);

		++keyIndex;
	}
}

static void copyKeySetToMmap (char * dest, KeySet * keySet, MmapHeader * mmapHeader, MmapMetaData * mmapMetaData, MmapFooter * mmapFooter,
			      DynArray * dynArray)
{
	mmapMetaData->destAddr = dest;
	writeMagicData (dest);

	MmapAddr mmapAddr = { .ksPtr = (KeySet *) (dest + SIZEOF_MMAPHEADER + SIZEOF_MMAPMETADATA + SIZEOF_KEYSET + SIZEOF_KEY),
			      .metaKsPtr = (char *) mmapAddr.ksPtr + SIZEOF_KEYSET,
			      .ksArrayPtr = (((char *) mmapAddr.ksPtr) + (SIZEOF_KEYSET * mmapMetaData->numKeySets)),
			      .metaKsArrayPtr = mmapAddr.ksArrayPtr + (SIZEOF_KEY_PTR * keySet->alloc),
			      .keyPtr = (mmapAddr.ksArrayPtr + (SIZEOF_KEY_PTR * mmapMetaData->ksAlloc)),
			      .dataPtr = ((char *) mmapAddr.keyPtr + (SIZEOF_KEY * mmapMetaData->numKeys)),
			      .mmapAddrInt = (uintptr_t) mmapMetaData->destAddr };

	if (keySet->size != 0)
	{
		// first write the meta keys into place
		writeMetaKeys (&mmapAddr, dynArray);
		// then write Keys including meta KeySets
		writeKeys (keySet, &mmapAddr, dynArray);
	}

	mmapAddr.ksPtr->flags = KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
	mmapAddr.ksPtr->array = (Key **) mmapAddr.ksArrayPtr;
	mmapAddr.ksPtr->array[keySet->size] = 0;
	mmapAddr.ksPtr->array = (Key **) ((char *) mmapAddr.ksArrayPtr - mmapAddr.mmapAddrInt);
	mmapAddr.ksPtr->alloc = keySet->alloc;
	mmapAddr.ksPtr->size = keySet->size;
	mmapAddr.ksPtr->cursor = 0;
	mmapAddr.ksPtr->current = 0;
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	mmapAddr.ksPtr->opmphm = 0;
#endif

	memcpy ((dest + SIZEOF_MMAPHEADER), mmapMetaData, SIZEOF_MMAPMETADATA);
#ifdef ENABLE_MMAP_CHECKSUM
	uint32_t checksum = checksum = crc32 (0L, Z_NULL, 0);
	checksum = crc32 (checksum, (const unsigned char *) (dest + SIZEOF_MMAPHEADER), mmapHeader->cksumSize);
	mmapHeader->checksum = checksum;
#endif
	memcpy (dest, mmapHeader, SIZEOF_MMAPHEADER);
	memcpy ((dest + mmapHeader->allocSize - SIZEOF_MMAPFOOTER), mmapFooter, SIZEOF_MMAPFOOTER);
}

static void mmapToKeySet (char * mappedRegion, KeySet * returned)
{
	KeySet * keySet = (KeySet *) (mappedRegion + SIZEOF_MMAPHEADER + SIZEOF_MMAPMETADATA + SIZEOF_KEYSET + SIZEOF_KEY);
	returned->array = keySet->array;
	returned->size = keySet->size;
	returned->alloc = keySet->alloc;
	returned->cursor = 0;
	returned->current = 0;
	returned->mmapMetaData = (MmapMetaData *) (mappedRegion + SIZEOF_MMAPHEADER);
	// to be able to free() the returned KeySet, just set the array flag here
	returned->flags = KS_FLAG_MMAP_ARRAY;
	// we intentionally to not change the KeySet->opmphm here!
}

static void updatePointers (MmapMetaData * mmapMetaData, char * dest)
{
	uintptr_t destInt = (uintptr_t) dest;

	char * ksPtr = (dest + SIZEOF_MMAPHEADER + SIZEOF_MMAPMETADATA + SIZEOF_KEYSET + SIZEOF_KEY);

	char * ksArrayPtr = ksPtr + SIZEOF_KEYSET * mmapMetaData->numKeySets;
	char * keyPtr = ksArrayPtr + SIZEOF_KEY_PTR * mmapMetaData->ksAlloc;

	KeySet * ks;
	for (size_t i = 0; i < mmapMetaData->numKeySets; ++i)
	{
		ks = (KeySet *) ksPtr;
		ksPtr += SIZEOF_KEYSET;
		if (ks->array)
		{
			ks->array = (Key **) ((char *) ks->array + destInt);

			for (size_t j = 0; j < ks->size; ++j)
			{
				if (ks->array[j])
				{
					ks->array[j] = (Key *) ((char *) (ks->array[j]) + destInt);
				}
			}
		}
	}

	Key * key;
	for (size_t i = 0; i < mmapMetaData->numKeys; ++i)
	{
		key = (Key *) keyPtr;
		keyPtr += SIZEOF_KEY;
		if (key->data.v)
		{
			key->data.v = (void *) ((char *) (key->data.v) + destInt);
		}
		if (key->key)
		{
			key->key = ((char *) (key->key) + destInt);
		}
		if (key->meta)
		{
			key->meta = (KeySet *) ((char *) (key->meta) + destInt);
		}
	}
}

static void * hexStringToAddress (const char * hexString)
{
	int hexBase = 16;
	int errnosave = errno;
	char * endptr;
	errno = 0;

	unsigned long int val = strtoul (hexString, &endptr, hexBase);

	if ((errno != 0) || (endptr == hexString) || (*endptr != '\0'))
	{
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		errno = errnosave;
		return (void *) 0;
	}

	errno = errnosave;
	return (void *) val;
}

/**
 * @brief Deeply copies from source to dest.
 *
 * Creates a deep copy of the keyset, but does not duplicate
 * the opmphm structures.
 *
 * @param source has to be an initialized source KeySet
 * @return a deep copy of source on success
 * @retval 0 on NULL pointer or a memory error happened
 */
KeySet * mmapKsDeepDup (const KeySet * source)
{
	if (!source) return 0;

	size_t s = source->size;
	size_t i = 0;
	KeySet * keyset = 0;

	keyset = ksNew (source->alloc, KS_END);
	for (i = 0; i < s; ++i)
	{
		Key * k = source->array[i];
		Key * d = keyDup (k);
		if (!test_bit (k->flags, KEY_FLAG_SYNC))
		{
			keyClearSync (d);
		}
		if (ksAppendKey (keyset, d) == -1)
		{
			ksDel (keyset);
			return 0;
		}
	}

	return keyset;
}

static KeySet * copyKeySet (KeySet * toCopy, MmapMetaData * mmapMetaData)
{
	if (!mmapMetaData)
	{
		return 0;
	}

	if (test_bit (mmapMetaData->flags, MMAP_FLAG_DELETED) == MMAP_FLAG_DELETED)
	{
		return 0;
	}

	DynArray * dynArray = newDynArray ();

	Key * cur = 0;
	ksRewind (toCopy);
	while ((cur = ksNext (toCopy)) != 0)
	{
		if (cur->meta)
		{
			Key * curMetaKey;
			ksRewind (cur->meta);
			while ((curMetaKey = ksNext (cur->meta)) != 0)
			{
				if (findOrInsert (curMetaKey, dynArray) == 0)
				{
					// key was just inserted
				}
			}
		}
	}

	// allocate space in DynArray to remember the addresses of meta keys
	if (dynArray->size > 0)
	{
		dynArray->mappedKeyArray = elektraCalloc (dynArray->size * sizeof (Key *));
	}

	// duplicate all unique meta keys
	Key * copy = 0;
	cur = 0;
	for (size_t i = 0; i < dynArray->size; ++i)
	{
		cur = dynArray->keyArray[i];
		copy = keyDup (cur);

		dynArray->mappedKeyArray[i] = copy;
	}

	// duplicate keyset from mmap and replace meta keys with deep copy
	KeySet * dest = mmapKsDeepDup (toCopy);
	cur = 0;
	ksRewind (dest);
	while ((cur = ksNext (dest)) != 0)
	{
		if (cur->meta)
		{
			KeySet * newMetaKs = ksNew (cur->meta->alloc, KS_END);

			ksRewind (cur->meta);
			for (size_t i = 0; i < cur->meta->size; ++i)
			{
				Key * newMetaKey = dynArray->mappedKeyArray[find ((Key *) cur->meta->array[i], dynArray)];
				ksAppendKey (newMetaKs, newMetaKey);
			}
			ksDel (cur->meta);
			cur->meta = newMetaKs;
		}
	}

	delDynArray (dynArray);
	return (KeySet *) dest;
}

static void unlinkFile (Key * parentKey)
{
	ELEKTRA_LOG_DEBUG ("unlink: need to unlink old mapped memory from file");

	const Key * cur;
	keyRewindMeta (parentKey);
	while ((cur = keyNextMeta (parentKey)) != 0)
	{
		void * toUnlinkMmap = hexStringToAddress (keyName (cur));
		ELEKTRA_LOG_DEBUG ("unlink: unlinking mmap str: %s", keyName (cur));
		ELEKTRA_LOG_DEBUG ("unlink: unlinking mmap ptr: %p", toUnlinkMmap);

		void * toUnlinkMmapMetaData = (char *) toUnlinkMmap + SIZEOF_MMAPHEADER;
		ELEKTRA_LOG_DEBUG ("unlink: unlinking mmap meta-data ptr: %p", toUnlinkMmapMetaData);

		void * toUnlinkKS = hexStringToAddress (keyString (cur));
		ELEKTRA_LOG_DEBUG ("unlink: unlinking KeySet str: %s", keyString (cur));
		ELEKTRA_LOG_DEBUG ("unlink: unlinking KeySet ptr: %p", toUnlinkKS);

		KeySet * copy = copyKeySet (toUnlinkKS, toUnlinkMmapMetaData);
		if (copy)
		{
			KeySet * keySet = (KeySet *) toUnlinkKS;
			ksClose (keySet);
			keySet->array = copy->array;
			keySet->size = copy->size;
			keySet->alloc = copy->alloc;
			keySet->cursor = 0;
			keySet->current = 0;
			keySet->mmapMetaData = 0;
			keySet->flags = 0;
			// keySet->opmphm invalidated by ksClose already
			elektraFree (copy);
		}
	}
}

static void saveLinkedFile (Key * found, KeySet * mappedFiles, KeySet * returned, Plugin * handle, char * mappedRegion)
{
	char mmapAddrString[SIZEOF_ADDR_STRING];
	snprintf (mmapAddrString, SIZEOF_ADDR_STRING - 1, "%p", (void *) (mappedRegion));
	mmapAddrString[SIZEOF_ADDR_STRING - 1] = '\0';
	ELEKTRA_LOG_DEBUG ("mappedRegion ptr as string: %s", mmapAddrString);
	char ksAddrString[SIZEOF_ADDR_STRING];
	snprintf (ksAddrString, SIZEOF_ADDR_STRING - 1, "%p", (void *) returned);
	ksAddrString[SIZEOF_ADDR_STRING - 1] = '\0';
	ELEKTRA_LOG_DEBUG ("KeySet ptr as string: %s", ksAddrString);
	keySetMeta (found, mmapAddrString, ksAddrString);
	ksAppendKey (mappedFiles, found);
	elektraPluginSetData (handle, mappedFiles);
}

/* -- Exported Elektra Plugin Functions ------------------------------------------------------------------------------------------------- */

int elektraMmapstorageOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	KeySet * mappedFiles = ksNew (0, KS_END);
	elektraPluginSetData (handle, mappedFiles);

	const uintptr_t magicNumber = generateMagicNumber ();
	if (magicKeySet.array == 0) initMagicKeySet (magicNumber);
	if (magicKey.data.v == 0) initMagicKey (magicNumber);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	KeySet * mappedFiles = (KeySet *) elektraPluginGetData (handle);

	// unlink all remaining files!
	Key * cur = 0;
	ksRewind (mappedFiles);
	while ((cur = ksNext (mappedFiles)) != 0)
	{
		unlinkFile (cur);
	}
	ksDel (mappedFiles);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// get all keys
	int errnosave = errno;

	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/mmapstorage"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/mmapstorage", KEY_VALUE, "mmapstorage plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/mmapstorage/exports", KEY_END),
			keyNew ("system/elektra/modules/mmapstorage/exports/open", KEY_FUNC, elektraMmapstorageOpen, KEY_END),
			keyNew ("system/elektra/modules/mmapstorage/exports/close", KEY_FUNC, elektraMmapstorageClose, KEY_END),
			keyNew ("system/elektra/modules/mmapstorage/exports/get", KEY_FUNC, elektraMmapstorageGet, KEY_END),
			keyNew ("system/elektra/modules/mmapstorage/exports/set", KEY_FUNC, elektraMmapstorageSet, KEY_END),
#include ELEKTRA_README (mmapstorage)
			keyNew ("system/elektra/modules/mmapstorage/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	KeySet * mappedFiles = (KeySet *) elektraPluginGetData (handle);
	Key * found = ksLookup (mappedFiles, parentKey, 0);
	if (!found)
	{
		ELEKTRA_LOG_DEBUG ("unlink: new file, adding to my list");
		found = keyDup (parentKey);
	}

	FILE * fp;
	char * mappedRegion = MAP_FAILED;

	if ((fp = openFile (parentKey, "r+")) == 0)
	{
		goto error;
	}

	struct stat sbuf;
	if (statFile (&sbuf, parentKey) != 1)
	{
		goto error;
	}

	if (sbuf.st_size == 0)
	{
		// empty mmap file
		fclose (fp);
		keyDel (found);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	MmapHeader mmapHeader;
	MmapMetaData mmapMetaData;
	if (readHeader (fp, &mmapHeader, &mmapMetaData) == -1)
	{
		// config file was corrupt
		ELEKTRA_LOG_WARNING ("could not read mmap information header");
		goto error;
	}

	if (sbuf.st_size < 0 || (size_t) sbuf.st_size != mmapHeader.allocSize)
	{
		// config file size mismatch
		ELEKTRA_LOG_WARNING ("mmap file size differs from metadata, file was altered");
		goto error;
	}

	mappedRegion = mmapFile ((void *) 0, fp, sbuf.st_size, MAP_PRIVATE, parentKey);

	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("mappedRegion == MAP_FAILED");
		goto error;
	}

	if (readFooter (mappedRegion, &mmapHeader) == -1)
	{
		// config file was corrupt/truncated
		ELEKTRA_LOG_WARNING ("could not read mmap information footer: file was altered");
		goto error;
	}

#ifdef ENABLE_MMAP_CHECKSUM
	if (verifyChecksum (mappedRegion, &mmapHeader) != 0)
	{
		ELEKTRA_LOG_WARNING ("checksum failed");
		goto error;
	}
#endif

	if (verifyMagicData (mappedRegion) != 0)
	{
		// magic data could not be read properly, indicating unreadable format or different architecture
		ELEKTRA_LOG_WARNING ("mmap magic data could not be read properly");
		goto error;
	}

	ksClose (returned);
	updatePointers (&mmapMetaData, mappedRegion);
	mmapToKeySet (mappedRegion, returned);

	if (fclose (fp) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not fclose");
		goto error;
	}

	// save keyset information to list of currently mmaped files
	saveLinkedFile (found, mappedFiles, returned, handle, mappedRegion);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;

error:
	if (errno != 0)
	{
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		ELEKTRA_SET_ERROR_GET (parentKey);
	}

	if (mappedRegion != MAP_FAILED)
	{
		munmap (mappedRegion, sbuf.st_size);
	}
	fclose (fp);
	keyDel (found);

	errno = errnosave;
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

int elektraMmapstorageSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// set all keys
	int errnosave = errno;
	DynArray * dynArray = 0;

	// check if file has open mappings and unlink them
	KeySet * mappedFiles = (KeySet *) elektraPluginGetData (handle);
	Key * found = ksLookup (mappedFiles, parentKey, KDB_O_POP);
	if (found)
	{
		unlinkFile (found);
		keyDel (found);
	}

	FILE * fp;
	char * mappedRegion = MAP_FAILED;

	if ((fp = openFile (parentKey, "w+")) == 0)
	{
		goto error;
	}

	dynArray = newDynArray ();

	MmapHeader mmapHeader;
	MmapMetaData mmapMetaData;
	initHeader (&mmapHeader);
	initMetaData (&mmapMetaData);
	calculateMmapDataSize (&mmapHeader, &mmapMetaData, returned, dynArray);
	ELEKTRA_LOG_DEBUG ("mmapsize: %zu", mmapHeader.allocSize);

	if (truncateFile (fp, mmapHeader.allocSize, parentKey) != 1)
	{
		goto error;
	}

	mappedRegion = mmapFile ((void *) 0, fp, mmapHeader.allocSize, MAP_SHARED, parentKey);
	ELEKTRA_LOG_DEBUG ("mappedRegion ptr: %p", (void *) mappedRegion);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("mappedRegion == MAP_FAILED");
		goto error;
	}

	MmapFooter mmapFooter;
	initFooter (&mmapFooter);
	copyKeySetToMmap (mappedRegion, returned, &mmapHeader, &mmapMetaData, &mmapFooter, dynArray);
	if (msync ((void *) mappedRegion, mmapHeader.allocSize, MS_SYNC) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not msync");
		goto error;
	}

	if (munmap (mappedRegion, mmapHeader.allocSize) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not munmap");
		goto error;
	}

	if (fclose (fp) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not fclose");
		goto error;
	}

	delDynArray (dynArray);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;

error:
	if (errno != 0)
	{
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		ELEKTRA_SET_ERROR_SET (parentKey);
	}

	if (mappedRegion != MAP_FAILED)
	{
		munmap (mappedRegion, mmapHeader.allocSize);
	}
	fclose (fp);
	delDynArray (dynArray);

	errno = errnosave;
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (mmapstorage)
{
	// clang-format off
	return elektraPluginExport ("mmapstorage",
		ELEKTRA_PLUGIN_OPEN,	&elektraMmapstorageOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraMmapstorageClose,
		ELEKTRA_PLUGIN_GET,	&elektraMmapstorageGet,
		ELEKTRA_PLUGIN_SET,	&elektraMmapstorageSet,
		ELEKTRA_PLUGIN_END);
}
