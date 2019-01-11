/**
 * @file
 *
 * @brief Source for mmapstorage plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#define _XOPEN_SOURCE 600

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "mmapstorage.h"
#include "dynarray.h"
#include "internal.h"

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbprivate.h>

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <errno.h>
#include <fcntl.h>     // fcntl()
#include <limits.h>    // SSIZE_MAX
#include <stdint.h>    // uintN_t, uintptr_t
#include <stdio.h>     // fopen(), fileno()
#include <stdlib.h>    // strtol()
#include <string.h>    // memcmp()
#include <sys/mman.h>  // mmap()
#include <sys/stat.h>  // stat()
#include <sys/types.h> // ftruncate (), size_t
#include <unistd.h>    // close(), ftruncate(), unlink()

#ifdef ELEKTRA_MMAP_CHECKSUM
#include <zlib.h> // crc32()
#endif

/* -- Global declarations---------------------------------------------------------------------------------------------------------------- */

static KeySet magicKeySet;
static Key magicKey;
static MmapMetaData magicMmapMetaData;

/* -- File handling --------------------------------------------------------------------------------------------------------------------- */

/**
 * @brief Wrapper for open().
 *
 * @param parentKey containing the filename
 * @param flags file access mode
 * @param mode file mode bits when file is created
 *
 * @return file descriptor
 */
static int openFile (Key * parentKey, int flag, mode_t mode)
{
	int fd;
	ELEKTRA_LOG_DEBUG ("opening file %s", keyString (parentKey));

	if ((fd = open (keyString (parentKey), flag, mode)) == -1)
	{
		ELEKTRA_LOG_WARNING ("error opening file %s", keyString (parentKey));
	}
	return fd;
}

/**
 * @brief Wrapper for ftruncate().
 *
 * @param fd the file descriptor
 * @param mmapsize size of the mapped region
 * @param parentKey holding the filename, for debug purposes
 *
 * @retval 1 on success
 * @retval -1 if ftruncate() failed
 */
static int truncateFile (int fd, size_t mmapsize, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG_DEBUG ("truncating file %s", keyString (parentKey));

	if ((ftruncate (fd, mmapsize)) == -1)
	{
		ELEKTRA_LOG_WARNING ("error truncating file %s", keyString (parentKey));
		return -1;
	}
	return 1;
}

/**
 * @brief Wrapper for stat().
 *
 * @param sbuf the stat structure
 * @param parentKey holding the filename
 *
 * @retval 1 on success
 * @retval -1 if stat() failed
 */
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

/**
 * @brief Wrapper for mmap().
 *
 * @param addr address hint, where the mapping should start
 * @param fd file descriptor of the file to be mapped
 * @param mmapSize size of the mapped region
 * @param mapOpts mmap flags (MAP_PRIVATE, MAP_FIXED, ...)
 * @param parentKey holding the filename, for debug purposes
 *
 * @return pointer to mapped region on success, MAP_FAILED on failure
 */
static char * mmapFile (void * addr, int fd, size_t mmapSize, int mapOpts, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG_DEBUG ("mapping file %s", keyString (parentKey));

	char * mappedRegion = mmap (addr, mmapSize, PROT_READ | PROT_WRITE, mapOpts, fd, 0);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("error mapping file %s\nmmapSize: %zu", keyString (parentKey), mmapSize);
		return MAP_FAILED;
	}
	return mappedRegion;
}

/* -- Internal Functions  --------------------------------------------------------------------------------------------------------------- */

/**
 * @brief MmapHeader initializer.
 *
 * @param mmapHeader to initialize
 */
static void initHeader (MmapHeader * mmapHeader)
{
	memset (mmapHeader, 0, SIZEOF_MMAPHEADER);
	mmapHeader->mmapMagicNumber = ELEKTRA_MAGIC_MMAP_NUMBER;
	mmapHeader->formatVersion = ELEKTRA_MMAP_FORMAT_VERSION;
#ifdef ELEKTRA_MMAP_CHECKSUM
	mmapHeader->formatFlags = ELEKTRA_MMAP_CHECKSUM_ON;
#endif
}

/**
 * @brief MmapMetaData initializer.
 *
 * @param mmapMetaData to initialize
 */
static void initMetaData (MmapMetaData * mmapMetaData)
{
	memset (mmapMetaData, 0, SIZEOF_MMAPMETADATA);
}

/**
 * @brief MmapFooter initializer.
 *
 * @param mmapFooter to initialize
 */
static void initFooter (MmapFooter * mmapFooter)
{
	memset (mmapFooter, 0, SIZEOF_MMAPFOOTER);
	mmapFooter->mmapMagicNumber = ELEKTRA_MAGIC_MMAP_NUMBER;
}

/**
 * @brief Reads the MmapHeader and MmapMetaData from a file.
 *
 * @param fp file pointer to read the data from
 * @param mmapHeader buffer where the MmapHeader is stored
 * @param mmapMetaData buffer where the MmapMetaData is stored
 *
 * @retval -1 if magic number or format version was wrong
 * @retval 0 if read succeeded, the magic number was read correctly and the format version matched
 */
static int readHeader (const char * mappedRegion, MmapHeader ** mmapHeader, MmapMetaData ** mmapMetaData)
{
	*mmapHeader = (MmapHeader *) mappedRegion;
	*mmapMetaData = (MmapMetaData *) (mappedRegion + OFFSET_REAL_MMAPMETADATA);

	if ((*mmapHeader)->mmapMagicNumber == ELEKTRA_MAGIC_MMAP_NUMBER && (*mmapHeader)->formatVersion == ELEKTRA_MMAP_FORMAT_VERSION)
	{
		return 0;
	}

	return -1;
}

/**
 * @brief Reads the MmapFooter from the end of the mapped region.
 *
 * @param mappedRegion pointer to the mapped region
 * @param mmapHeader the MmapHeader containing the size of the allocation
 *
 * @retval -1 if the magic number did not match
 * @retval 0 if the magic number matched
 */
static int readFooter (const char * mappedRegion, MmapHeader * mmapHeader)
{
	MmapFooter * mmapFooter = (MmapFooter *) (mappedRegion + mmapHeader->allocSize - SIZEOF_MMAPFOOTER);

	if (mmapFooter->mmapMagicNumber == ELEKTRA_MAGIC_MMAP_NUMBER)
	{
		return 0;
	}

	return -1;
}

/**
 * @brief Writes the magic data for consistency checks.
 *
 * @param dest to write the data to.
 */
static void writeMagicData (const char * mappedRegion)
{
	KeySet * destKeySet = (KeySet *) (mappedRegion + OFFSET_MAGIC_KEYSET);
	Key * keyPtr = (Key *) (mappedRegion + OFFSET_MAGIC_KEY);
	MmapMetaData * mmapMetaData = (MmapMetaData *) (mappedRegion + OFFSET_MAGIC_MMAPMETADATA);
	memcpy (destKeySet, &magicKeySet, SIZEOF_KEYSET);
	memcpy (keyPtr, &magicKey, SIZEOF_KEY);
	memcpy (mmapMetaData, &magicMmapMetaData, SIZEOF_MMAPMETADATA);
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

/**
 * @brief Magic KeySet initializer.
 *
 * @param magicNumber to detect arbitrary byte-swaps
 */
static void initMagicKeySet (const uintptr_t magicNumber)
{
	magicKeySet.array = (Key **) magicNumber;
	magicKeySet.size = SIZE_MAX;
	magicKeySet.alloc = 0;
	magicKeySet.cursor = (Key *) ~magicNumber;
	magicKeySet.current = SIZE_MAX / 2;
	magicKeySet.flags = KS_FLAG_MMAP_ARRAY | KS_FLAG_SYNC;
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	magicKeySet.opmphm = (Opmphm *) ELEKTRA_MMAP_MAGIC_BOM;
	magicKeySet.opmphmPredictor = 0;
#endif
}

/**
 * @brief Magic Key initializer.
 *
 * @param magicNumber to detect arbitrary byte-swaps
 */
static void initMagicKey (const uintptr_t magicNumber)
{
	magicKey.data.v = (void *) ~magicNumber;
	magicKey.dataSize = SIZE_MAX;
	magicKey.key = (char *) magicNumber;
	magicKey.keySize = UINT16_MAX;
	magicKey.keyUSize = 0;
	magicKey.flags = KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_DATA | KEY_FLAG_MMAP_KEY | KEY_FLAG_SYNC;
	magicKey.ksReference = SIZE_MAX / 2;
	magicKey.meta = (KeySet *) ELEKTRA_MMAP_MAGIC_BOM;
}

/**
 * @brief Magic MmapMetaData initializer.
 *
 * @param magicNumber to detect arbitrary byte-swaps
 */
static void initMagicMmapMetaData (const uintptr_t magicNumber)
{
	magicMmapMetaData.destAddr = (char *) magicNumber;
	magicMmapMetaData.numKeySets = SIZE_MAX;
	magicMmapMetaData.ksAlloc = 0;
	magicMmapMetaData.numKeys = SIZE_MAX / 2;
}

/**
 * @brief Verify the magic KeySet.
 *
 * @param ks keyset to verify
 *
 * @retval 0 if magic KeySet is consistent
 * @retval -1 if magic KeySet is inconsistent
 */
static int verifyMagicKeySet (KeySet * ks)
{
	if (!ks) return -1;
	return memcmp (ks, &magicKeySet, SIZEOF_KEYSET);
}

/**
 * @brief Verify the magic Key.
 *
 * @param key to verify
 *
 * @retval 0 if magic Key is consistent
 * @retval -1 if magic Key is inconsistent
 */
static int verifyMagicKey (Key * key)
{
	if (!key) return -1;
	return memcmp (key, &magicKey, SIZEOF_KEY);
}

/**
 * @brief Verify the magic MmapMetaData.
 *
 * @param mmapMetaData to verify
 *
 * @retval 0 if magic MmapMetaData is consistent
 * @retval -1 if magic MmapMetaData is inconsistent
 */
static int verifyMagicMmapMetaData (MmapMetaData * mmapMetaData)
{
	if (!mmapMetaData) return -1;
	return memcmp (mmapMetaData, &magicMmapMetaData, SIZEOF_MMAPMETADATA);
}

/**
 * @brief Verify magic data in the mapped region.
 *
 * @param mappedRegion pointer to mapped region
 *
 * @retval 0 if magic data is consistent
 * @retval -1 if magic data is inconsistent
 */
static int verifyMagicData (char * mappedRegion)
{
	KeySet * destKeySet = (KeySet *) (mappedRegion + OFFSET_MAGIC_KEYSET);
	Key * keyPtr = (Key *) (mappedRegion + OFFSET_MAGIC_KEY);
	MmapMetaData * mmapMetaData = (MmapMetaData *) (mappedRegion + OFFSET_MAGIC_MMAPMETADATA);

	if ((verifyMagicKey (keyPtr) != 0) || (verifyMagicKeySet (destKeySet) != 0) || (verifyMagicMmapMetaData (mmapMetaData) != 0))
	{
		return -1;
	}

	return 0;
}

#ifdef ELEKTRA_MMAP_CHECKSUM
/**
 * @brief Verify checksum of the critical mmap data.
 *
 * Verifies the CRC32 checksum of all KeySet and Key structs (including pointers/pointer arrays)
 * as well as the MmapMetaData. Does not check Key name and value.
 *
 * @param mappedRegion pointer to mapped region
 * @param mmapHeader containing the stored checksum and size of the checksum region
 *
 * @retval 0 if checksum was correct
 * @retval -1 if there was a checksum mismatch
 */
static int verifyChecksum (char * mappedRegion, MmapHeader * mmapHeader)
{
	// if file was written without checksum, we skip the check
	if (!test_bit (mmapHeader->formatFlags, ELEKTRA_MMAP_CHECKSUM_ON)) return 0;

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

/**
 * @brief Calculates the size, in bytes, needed to store the KeySet in a mmap region.
 *
 * Iterates over the KeySet and calculates the complete size in bytes, needed to store the KeySet
 * within a mapped region. The size includes all mmap meta-information, magic KeySet and Key for
 * consistency checks, KeySets, meta-KeySets, Keys, meta-Keys, Key names and values.
 * Copied meta-Keys are counted once for deduplication. If needed, padding is added to align the
 * MmapFooter properly at the end of the mapping.
 *
 * The complete size and some other meta-information are stored in the MmapHeader and MmapMetaData.
 * The DynArray stores the unique meta-Key pointers needed for deduplication.
 *
 * @param mmapHeader to store the allocation size
 * @param mmapMetaData to store the number of KeySets and Keys
 * @param returned the KeySet that should be stored
 * @param dynArray to store meta-key pointers for deduplication
 */
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
				if (ELEKTRA_PLUGIN_FUNCTION (mmapstorage, dynArrayFindOrInsert) (curMeta, dynArray) == 0)
				{
					// key was just inserted
					dataBlocksSize += (curMeta->keySize + curMeta->keyUSize + curMeta->dataSize);
				}
			}
			metaKsAlloc += (cur->meta->alloc);
		}
	}

	size_t keyArraySize = (returned->size + 1) * SIZEOF_KEY; // +1 for magic Key
	size_t keyPtrArraySize = (returned->alloc) * SIZEOF_KEY_PTR;

	// SIZEOF_KEYSET * 2 : magic KeySet + main KeySet
	size_t allocSize = (SIZEOF_KEYSET * 2) + keyPtrArraySize + keyArraySize + dataBlocksSize + (metaKeySets * SIZEOF_KEYSET) +
			   (metaKsAlloc * SIZEOF_KEY_PTR) + (dynArray->size * SIZEOF_KEY);
	mmapHeader->cksumSize = allocSize + (SIZEOF_MMAPMETADATA * 2); // cksumSize now contains size of all critical data

	size_t padding = sizeof (uint64_t) - (allocSize % sizeof (uint64_t)); // alignment for MMAP Footer at end of mapping
	allocSize += SIZEOF_MMAPHEADER + (SIZEOF_MMAPMETADATA * 2) + SIZEOF_MMAPFOOTER + padding;

	mmapHeader->allocSize = allocSize;
	mmapMetaData->numKeySets = 1 + metaKeySets; // 1: main KeySet
	mmapMetaData->ksAlloc = returned->alloc + metaKsAlloc;
	mmapMetaData->numKeys = returned->size + dynArray->size;
}

/**
 * @brief Write meta-keys to the mapped region.
 *
 * This function only writes all meta-keys to the mapped region.
 * The meta-keys are later referenced in the meta-keysets.
 *
 * @param mmapAddr struct containing pointers to the mapped region
 * @param dynArray containing deduplicated meta-keys
 */
static void writeMetaKeys (MmapAddr * mmapAddr, DynArray * dynArray)
{
	// allocate space in DynArray to remember the addresses of mapped meta-keys
	if (dynArray->size > 0)
	{
		dynArray->mappedKeyArray = elektraCalloc (dynArray->size * sizeof (Key *));
	}

	// write the meta keys into place
	for (size_t i = 0; i < dynArray->size; ++i)
	{
		Key * curMeta = dynArray->keyArray[i];	// old key location
		Key * mmapMetaKey = (Key *) mmapAddr->keyPtr; // new key location
		*mmapMetaKey = *curMeta;
		mmapAddr->keyPtr += SIZEOF_KEY;

		// move Key name
		if (curMeta->key)
		{
			size_t keyNameSize = curMeta->keySize + curMeta->keyUSize;
			memcpy (mmapAddr->dataPtr, curMeta->key, keyNameSize);
			mmapMetaKey->key = (mmapAddr->dataPtr - mmapAddr->mmapAddrInt);
			mmapAddr->dataPtr += keyNameSize;
		}

		// move Key value
		if (curMeta->data.v)
		{
			memcpy (mmapAddr->dataPtr, curMeta->data.v, curMeta->dataSize);
			mmapMetaKey->data.v = (mmapAddr->dataPtr - mmapAddr->mmapAddrInt);
			mmapAddr->dataPtr += curMeta->dataSize;
		}

		// move Key itself
		mmapMetaKey->flags |= KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_KEY | KEY_FLAG_MMAP_DATA;
		mmapMetaKey->meta = 0;
		mmapMetaKey->ksReference = 0;

		dynArray->mappedKeyArray[i] = mmapMetaKey;
	}
}

/**
 * @brief Writes a meta keyset of a key to the mapped region.
 *
 * @param key holding the meta-keyset
 * @param mmapAddr structure holding pointers to the mapped region
 * @param dynArray holding deduplicated references to meta-keys
 *
 * @return pointer to the new meta keyset
 */
static KeySet * writeMetaKeySet (Key * key, MmapAddr * mmapAddr, DynArray * dynArray)
{
	// write the meta KeySet
	if (!key->meta) return 0;

	KeySet * newMeta = (KeySet *) mmapAddr->metaKsPtr;
	mmapAddr->metaKsPtr += SIZEOF_KEYSET;

	newMeta->flags = key->meta->flags | KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
	newMeta->array = (Key **) mmapAddr->metaKsArrayPtr;
	mmapAddr->metaKsArrayPtr += SIZEOF_KEY_PTR * key->meta->alloc;

	keyRewindMeta (key);
	size_t metaKeyIndex = 0;
	Key * mappedMetaKey = 0;
	const Key * metaKey;
	while ((metaKey = keyNextMeta (key)) != 0)
	{
		// get address of mapped key and store it in the new array
		mappedMetaKey = dynArray->mappedKeyArray[ELEKTRA_PLUGIN_FUNCTION (mmapstorage, dynArrayFind) ((Key *) metaKey, dynArray)];
		newMeta->array[metaKeyIndex] = (Key *) ((char *) mappedMetaKey - mmapAddr->mmapAddrInt);
		if (mappedMetaKey->ksReference < SSIZE_MAX)
		{
			++(mappedMetaKey->ksReference);
		}
		++metaKeyIndex;
	}
	newMeta->array[key->meta->size] = 0;
	newMeta->array = (Key **) ((char *) newMeta->array - mmapAddr->mmapAddrInt);
	newMeta->alloc = key->meta->alloc;
	newMeta->size = key->meta->size;
	newMeta = (KeySet *) ((char *) newMeta - mmapAddr->mmapAddrInt);
	return newMeta;
}

/**
 * @brief Writes the keys of the keyset to the mapped region.
 *
 * For each key, the corresponding meta keyset is also written
 * to the mapped region.
 *
 * @param keySet holding the keys to be written to the mapped region
 * @param mmapAddr structure holding pointers to the mapped region
 * @param dynArray holding deduplicated meta-key pointers
 */
static void writeKeys (KeySet * keySet, MmapAddr * mmapAddr, DynArray * dynArray)
{
	Key * cur;
	size_t keyIndex = 0;
	ksRewind (keySet);
	while ((cur = ksNext (keySet)) != 0)
	{
		Key * mmapKey = (Key *) mmapAddr->keyPtr; // new key location
		mmapAddr->keyPtr += SIZEOF_KEY;
		*mmapKey = *cur;

		// move Key name
		if (cur->key)
		{
			size_t keyNameSize = cur->keySize + cur->keyUSize;
			memcpy (mmapAddr->dataPtr, cur->key, keyNameSize);
			mmapKey->key = (mmapAddr->dataPtr - mmapAddr->mmapAddrInt);
			mmapAddr->dataPtr += keyNameSize;
		}

		// move Key value
		if (cur->data.v)
		{
			memcpy (mmapAddr->dataPtr, cur->data.v, cur->dataSize);
			mmapKey->data.v = (mmapAddr->dataPtr - mmapAddr->mmapAddrInt);
			mmapAddr->dataPtr += cur->dataSize;
		}

		// write the meta KeySet
		KeySet * newMeta = writeMetaKeySet (cur, mmapAddr, dynArray);

		// move Key itself
		mmapKey->flags |= KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_KEY | KEY_FLAG_MMAP_DATA;
		mmapKey->meta = newMeta;
		mmapKey->ksReference = 1;

		// write the relative Key pointer into the KeySet array
		((Key **) mmapAddr->ksArrayPtr)[keyIndex] = (Key *) ((char *) mmapKey - mmapAddr->mmapAddrInt);

		++keyIndex;
	}
}

/**
 * @brief Copies a keyset to a mapped region.
 *
 * Writes a keyset with all needed information to the mapped region and calculates
 * the checksum. Everything is written to the mapped region, including keyset, keys,
 * meta-information and data for consistency checks.
 *
 * @param dest address of the mapped region
 * @param keySet to copy to the mapped region
 * @param mmapHeader containing the size and checksum of the mapped region
 * @param mmapMetaData containing meta-information of the mapped region
 * @param mmapFooter containing a magic number for consistency checks
 * @param dynArray containing deduplicated pointers to meta-keys
 */
static void copyKeySetToMmap (char * dest, KeySet * keySet, MmapHeader * mmapHeader, MmapMetaData * mmapMetaData, MmapFooter * mmapFooter,
			      DynArray * dynArray)
{
	mmapMetaData->destAddr = dest;
	writeMagicData (dest);

	MmapAddr mmapAddr = { .ksPtr = (KeySet *) (dest + OFFSET_REAL_KEYSET),
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

	mmapAddr.ksPtr->flags = keySet->flags | KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
	mmapAddr.ksPtr->array = (Key **) mmapAddr.ksArrayPtr;
	mmapAddr.ksPtr->array[keySet->size] = 0;
	mmapAddr.ksPtr->array = (Key **) ((char *) mmapAddr.ksArrayPtr - mmapAddr.mmapAddrInt);
	mmapAddr.ksPtr->alloc = keySet->alloc;
	mmapAddr.ksPtr->size = keySet->size;

	memcpy ((dest + OFFSET_REAL_MMAPMETADATA), mmapMetaData, SIZEOF_MMAPMETADATA);
#ifdef ELEKTRA_MMAP_CHECKSUM
	uint32_t checksum = checksum = crc32 (0L, Z_NULL, 0);
	checksum = crc32 (checksum, (const unsigned char *) (dest + SIZEOF_MMAPHEADER), mmapHeader->cksumSize);
	mmapHeader->checksum = checksum;
#endif
	memcpy (dest, mmapHeader, SIZEOF_MMAPHEADER);
	memcpy ((dest + mmapHeader->allocSize - SIZEOF_MMAPFOOTER), mmapFooter, SIZEOF_MMAPFOOTER);
}

/**
 * @brief Replaces contents of a keyset with the keyset from the mapped region.
 *
 * The keyset members are replaced with data from a mapped keyset. The keyset
 * array then points into the mapped region.
 *
 * @param mappedRegion pointer to mapped region, holding an already written keyset
 * @param returned keyset to be replaced by the mapped keyset
 */
static void mmapToKeySet (char * mappedRegion, KeySet * returned)
{
	KeySet * keySet = (KeySet *) (mappedRegion + OFFSET_REAL_KEYSET);
	returned->array = keySet->array;
	returned->size = keySet->size;
	returned->alloc = keySet->alloc;
	// to be able to free() the returned KeySet, just set the array flag here
	returned->flags = KS_FLAG_MMAP_ARRAY;
	// we intentionally do not change the KeySet->opmphm here!
}

/**
 * @brief Updates pointers of a mapped keyset to a new location in memory.
 *
 * After mapping a file to a new location, all pointers have to be updated
 * in order to be consistent. When the mapped keyset is written, we subtract
 * the base address. Therefore, after mapping the keyset to a new memory location,
 * we only have to add the new base address to all pointers.
 *
 * @param mmapMetaData meta-data of the old mapped region
 * @param dest new mapped memory region
 */
static void updatePointers (MmapMetaData * mmapMetaData, char * dest)
{
	uintptr_t destInt = (uintptr_t) dest;

	char * ksPtr = (dest + OFFSET_REAL_KEYSET);

	char * ksArrayPtr = ksPtr + SIZEOF_KEYSET * mmapMetaData->numKeySets;
	char * keyPtr = ksArrayPtr + SIZEOF_KEY_PTR * mmapMetaData->ksAlloc;

	for (size_t i = 0; i < mmapMetaData->numKeySets; ++i)
	{
		KeySet * ks = (KeySet *) ksPtr;
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

	for (size_t i = 0; i < mmapMetaData->numKeys; ++i)
	{
		Key * key = (Key *) keyPtr;
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

/* -- Exported Elektra Plugin Functions ------------------------------------------------------------------------------------------------- */

/**
 * @brief Initializes the plugin data and magic keyset and key.
 *
 * @param handle The plugin handle.
 * @param errorKey Unused.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR on memory error (plugin data (keyset) could not be allocated).
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if initialization was successful.
 */
int ELEKTRA_PLUGIN_FUNCTION (mmapstorage, open) (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic

	const uintptr_t magicNumber = generateMagicNumber ();
	if (magicKeySet.array == 0) initMagicKeySet (magicNumber);
	if (magicKey.data.v == 0) initMagicKey (magicNumber);
	if (magicMmapMetaData.destAddr == 0) initMagicMmapMetaData (magicNumber);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief Cleans up the plugin data and unlinks all mapped files.
 *
 * @param handle The plugin handle.
 * @param errorKey Unused.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS always.
 */
int ELEKTRA_PLUGIN_FUNCTION (mmapstorage, close) (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * @brief The mmapstorage get function loads a keyset from a file and returns it.
 *
 * On successful mapping the returned keyset is replaced by the mapped keyset.
 * The returned keyset array then points to a mapped region.
 *
 * @param handle The plugin handle.
 * @param ks The keyset which is replaced by the mapped keyset.
 * @param parentKey Holding the filename or error message.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the file was mapped successfully.
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if the file could not be mapped successfully.
 */
int ELEKTRA_PLUGIN_FUNCTION (mmapstorage, get) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
{
	// get all keys
	int errnosave = errno;

	Key * root = keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME, KEY_END);
	if (keyRel (root, parentKey) >= 0)
	{
		keyDel (root);
		KeySet * contract =
#include "contract.h"
			ksAppend (ks, contract);
		ksDel (contract);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	keyDel (root);

	int fd = -1;
	char * mappedRegion = MAP_FAILED;

	if ((fd = openFile (parentKey, O_RDONLY, 0)) == -1)
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
		if (close (fd) != 0)
		{
			ELEKTRA_LOG_WARNING ("could not close");
			goto error;
		}
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (sbuf.st_size < 0 || (size_t) sbuf.st_size < ELEKTRA_MMAP_MINSIZE)
	{
		// file is smaller than minimum size
		goto error;
	}

	mappedRegion = mmapFile ((void *) 0, fd, sbuf.st_size, MAP_PRIVATE, parentKey);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("mappedRegion == MAP_FAILED");
		goto error;
	}

	MmapHeader * mmapHeader;
	MmapMetaData * mmapMetaData;
	if (readHeader (mappedRegion, &mmapHeader, &mmapMetaData) == -1)
	{
		// config file was corrupt
		ELEKTRA_LOG_WARNING ("could not read mmap information header");
		goto error;
	}

	if (sbuf.st_size < 0 || (size_t) sbuf.st_size != mmapHeader->allocSize)
	{
		// config file size mismatch
		ELEKTRA_LOG_WARNING ("mmap file size differs from metadata, file was altered");
		goto error;
	}

	if (readFooter (mappedRegion, mmapHeader) == -1)
	{
		// config file was corrupt/truncated
		ELEKTRA_LOG_WARNING ("could not read mmap information footer: file was altered");
		goto error;
	}

#ifdef ELEKTRA_MMAP_CHECKSUM
	if (verifyChecksum (mappedRegion, mmapHeader) != 0)
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

	ksClose (ks);
	updatePointers (mmapMetaData, mappedRegion);
	mmapToKeySet (mappedRegion, ks);

	if (close (fd) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not close");
		goto error;
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;

error:
	if (errno != 0)
	{
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		ELEKTRA_SET_ERROR_GET (parentKey);
	}

	if ((mappedRegion != MAP_FAILED) && (munmap (mappedRegion, sbuf.st_size) != 0))
	{
		ELEKTRA_LOG_WARNING ("could not munmap");
	}
	if ((fd != -1) && close (fd) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not close");
	}

	errno = errnosave;
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

/**
 * @brief The mmapstorage set function writes a keyset to a new file.
 *
 * The keyset is written to a new file. If the file was mapped previously,
 * all current mappings are unlinked.
 *
 * @param handle The plugin handle.
 * @param ks The keyset to be written.
 * @param parentKey Holding the filename or error message.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the file was written successfully.
 * @retval ELEKTRA_PLUGIN_STATUS_ERROR if any error occurred.
 */
int ELEKTRA_PLUGIN_FUNCTION (mmapstorage, set) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
{
	// set all keys
	int errnosave = errno;
	int fd = -1;
	char * mappedRegion = MAP_FAILED;
	DynArray * dynArray = 0;

	if (unlink (keyString (parentKey)) != 0 && errno != ENOENT)
	{
		ELEKTRA_LOG_WARNING ("could not unlink");
		goto error;
	}

	if ((fd = openFile (parentKey, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR)) == -1)
	{
		goto error;
	}

	dynArray = ELEKTRA_PLUGIN_FUNCTION (mmapstorage, dynArrayNew) ();

	MmapHeader mmapHeader;
	MmapMetaData mmapMetaData;
	initHeader (&mmapHeader);
	initMetaData (&mmapMetaData);
	calculateMmapDataSize (&mmapHeader, &mmapMetaData, ks, dynArray);
	ELEKTRA_LOG_DEBUG ("mmapsize: %llu", mmapHeader.allocSize);

	if (truncateFile (fd, mmapHeader.allocSize, parentKey) != 1)
	{
		goto error;
	}

	mappedRegion = mmapFile ((void *) 0, fd, mmapHeader.allocSize, MAP_SHARED, parentKey);
	ELEKTRA_LOG_DEBUG ("mappedRegion ptr: %p", (void *) mappedRegion);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("mappedRegion == MAP_FAILED");
		goto error;
	}

	MmapFooter mmapFooter;
	initFooter (&mmapFooter);
	copyKeySetToMmap (mappedRegion, ks, &mmapHeader, &mmapMetaData, &mmapFooter, dynArray);
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

	if (close (fd) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not close");
		goto error;
	}

	ELEKTRA_PLUGIN_FUNCTION (mmapstorage, dynArrayDelete) (dynArray);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;

error:
	if (errno != 0)
	{
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		ELEKTRA_SET_ERROR_SET (parentKey);
	}

	if ((mappedRegion != MAP_FAILED) && (munmap (mappedRegion, mmapHeader.allocSize) != 0))
	{
		ELEKTRA_LOG_WARNING ("could not munmap");
	}
	if ((fd != -1) && close (fd) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not close");
	}

	ELEKTRA_PLUGIN_FUNCTION (mmapstorage, dynArrayDelete) (dynArray);

	errno = errnosave;
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (mmapstorage)
{
	// clang-format off
	return elektraPluginExport (ELEKTRA_PLUGIN_NAME,
		ELEKTRA_PLUGIN_OPEN,	&ELEKTRA_PLUGIN_FUNCTION(mmapstorage, open),
		ELEKTRA_PLUGIN_CLOSE,	&ELEKTRA_PLUGIN_FUNCTION(mmapstorage, close),
		ELEKTRA_PLUGIN_GET,	&ELEKTRA_PLUGIN_FUNCTION(mmapstorage, get),
		ELEKTRA_PLUGIN_SET,	&ELEKTRA_PLUGIN_FUNCTION(mmapstorage, set),
		ELEKTRA_PLUGIN_END);
}
