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
#include <sys/stat.h>  // stat(), fstat()
#include <sys/types.h> // ftruncate (), size_t
#include <unistd.h>    // close(), ftruncate(), unlink(), read(), write()

#ifdef ELEKTRA_MMAP_CHECKSUM
#include <zlib.h> // crc32()
#endif

/* -- Global declarations---------------------------------------------------------------------------------------------------------------- */

static KeySet magicKeySet;
static Key magicKey;
static MmapMetaData magicMmapMetaData;

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
static Opmphm magicOpmphm;
static OpmphmPredictor magicOpmphmPredictor;
#endif

typedef enum
{
	MODE_STORAGE = 1,
	MODE_GLOBALCACHE = 1 << 1,
	MODE_NONREGULAR_FILE = 1 << 2
} PluginMode;

/* -- File handling --------------------------------------------------------------------------------------------------------------------- */

/**
 * @brief Wrapper for open().
 *
 * @param parentKey containing the filename
 * @param flags file access mode
 * @param openMode file mode bits when file is created
 * @param mode the current plugin mode
 *
 * @return file descriptor
 */
static int openFile (Key * parentKey, int flag, mode_t openMode, PluginMode mode)
{
	int fd;
	ELEKTRA_LOG_DEBUG ("opening file %s", keyString (parentKey));

	if ((fd = open (keyString (parentKey), flag, openMode)) == -1)
	{
		ELEKTRA_MMAP_LOG_WARNING ("error opening file %s", keyString (parentKey));
	}
	return fd;
}

/**
 * @brief Wrapper for ftruncate().
 *
 * @param fd the file descriptor
 * @param mmapsize size of the mapped region
 * @param parentKey holding the filename, for debug purposes
 * @param mode the current plugin mode
 *
 * @retval 1 on success
 * @retval -1 if ftruncate() failed
 */
static int truncateFile (int fd, size_t mmapsize, Key * parentKey ELEKTRA_UNUSED, PluginMode mode)
{
	ELEKTRA_LOG_DEBUG ("truncating file %s", keyString (parentKey));

	if ((ftruncate (fd, mmapsize)) == -1)
	{
		ELEKTRA_MMAP_LOG_WARNING ("error truncating file %s", keyString (parentKey));
		return -1;
	}
	return 1;
}

/**
 * @brief Wrapper for fstat().
 *
 * @param fd the file descriptor
 * @param sbuf the stat structure
 * @param parentKey holding the filename
 * @param mode the current plugin mode
 *
 * @retval 1 on success
 * @retval -1 if fstat() failed
 */
static int fstatFile (int fd, struct stat * sbuf, Key * parentKey ELEKTRA_UNUSED, PluginMode mode)
{
	ELEKTRA_LOG_DEBUG ("fstat() on file %s", keyString (parentKey));

	memset (sbuf, 0, sizeof (struct stat));
	if (fstat (fd, sbuf) == -1)
	{
		ELEKTRA_MMAP_LOG_WARNING ("error on fstat() for file %s", keyString (parentKey));
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
 * @param mode the current plugin mode
 *
 * @return pointer to mapped region on success, MAP_FAILED on failure
 */
static char * mmapFile (void * addr, int fd, size_t mmapSize, int mapOpts, Key * parentKey ELEKTRA_UNUSED, PluginMode mode)
{
	ELEKTRA_LOG_DEBUG ("mapping file %s", keyString (parentKey));

	char * mappedRegion = MAP_FAILED;
	if (test_bit (mode, MODE_NONREGULAR_FILE))
	{
		mappedRegion = elektraCalloc (mmapSize);
		if (!mappedRegion)
		{
			ELEKTRA_MMAP_LOG_WARNING ("error allocating buffer for file %s\nmmapSize: %zu", keyString (parentKey), mmapSize);
			return MAP_FAILED;
		}
	}
	else
	{
		mappedRegion = mmap (addr, mmapSize, PROT_READ | PROT_WRITE, mapOpts, fd, 0);
		if (mappedRegion == MAP_FAILED)
		{
			ELEKTRA_MMAP_LOG_WARNING ("error mapping file %s\nmmapSize: %zu", keyString (parentKey), mmapSize);
			return MAP_FAILED;
		}
	}

	return mappedRegion;
}


/**
 * @brief Copy file
 *
 * @param sourceFd source file descriptor
 * @param destFd destination file descriptor
 *
 * @retval 0 on success
 * @retval -1 if any error occured
 */
static int copyFile (int sourceFd, int destFd)
{
	ssize_t readBytes = 0;
	char buf[ELEKTRA_MMAP_BUFSIZE];

	while ((readBytes = read (sourceFd, buf, ELEKTRA_MMAP_BUFSIZE)) > 0)
	{
		if (readBytes <= 0)
		{
			if (errno == EINTR)
				continue;
			else
				return -1;
		}

		ssize_t writtenBytes = 0;
		while (readBytes > 0)
		{
			writtenBytes = write (destFd, buf, readBytes);
			if (writtenBytes <= 0)
			{
				if (errno == EINTR)
					continue;
				else
					return -1;
			}
			readBytes -= writtenBytes;
		}
	}

	return 0;
}

/**
 * @brief Copy file to anonymous temporary file
 *
 * This function is needed to make mmapstorage compatible
 * with any file. The `mmap()` syscall only supports regular
 * files.
 *
 * @param fd source file descriptor
 * @param sbuf the stat structure
 * @param parentKey holding the file name
 * @param mode the current plugin mode
 *
 * @retval 0 on success
 * @retval -1 if any error occured
 */
static int copyToAnonymousTempfile (int fd, struct stat * sbuf, Key * parentKey, PluginMode mode)
{
	int tmpFd = 0;
	char name[] = ELEKTRA_MMAP_TMP_NAME;
	if ((tmpFd = mkstemp (name)) < 0)
	{
		return -1;
	}

	ELEKTRA_LOG_DEBUG ("using anon tmp file: %s", name);
	keySetString (parentKey, name);
	// use anonymous file
	if (unlink (name) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not unlink");
		return -1;
	}

	copyFile (fd, tmpFd);

	if (close (fd) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not close");
		return -1;
	}

	// replace old file
	fd = tmpFd;
	if (fstatFile (fd, sbuf, parentKey, mode) != 1)
	{
		return -1;
	}

	return fd;
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
	set_bit (mmapHeader->formatFlags, MMAP_FLAG_CHECKSUM);
#endif
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	set_bit (mmapHeader->formatFlags, MMAP_FLAG_OPMPHM);
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
	*mmapMetaData = (MmapMetaData *) (mappedRegion + OFFSET_MMAPMETADATA);

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
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	Opmphm * opmphm = (Opmphm *) (mappedRegion + OFFSET_MAGIC_OPMPHM);
	OpmphmPredictor * opmphmPredictor = (OpmphmPredictor *) (mappedRegion + OFFSET_MAGIC_OPMPHMPREDICTOR);
	memcpy (opmphm, &magicOpmphm, sizeof (Opmphm));
	memcpy (opmphmPredictor, &magicOpmphmPredictor, sizeof (OpmphmPredictor));
#endif
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
	magicKeySet.refs = UINT16_MAX;
	magicKeySet.reserved = 0;
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
	magicKey.ukey = 0;
	magicKey.keyUSize = 0;
	magicKey.meta = (KeySet *) ELEKTRA_MMAP_MAGIC_BOM;
	magicKey.flags = KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_DATA | KEY_FLAG_MMAP_KEY | KEY_FLAG_SYNC;
	magicKey.refs = UINT16_MAX / 2;
	magicKey.reserved = UINT16_MAX;
}

/**
 * @brief Magic MmapMetaData initializer.
 *
 * @param magicNumber to detect arbitrary byte-swaps
 */
static void initMagicMmapMetaData (void)
{
	magicMmapMetaData.numKeySets = SIZE_MAX;
	magicMmapMetaData.ksAlloc = 0;
	magicMmapMetaData.numKeys = SIZE_MAX / 2;
}

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
/**
 * @brief Magic Opmphm initializer.
 *
 * @param magicNumber to detect arbitrary byte-swaps
 */
static void initMagicOpmphm (const uintptr_t magicNumber)
{
	magicOpmphm.hashFunctionSeeds = (void *) magicNumber;
	magicOpmphm.rUniPar = INT8_MAX;
	magicOpmphm.componentSize = SIZE_MAX / 2;
	magicOpmphm.graph = (void *) ~magicNumber;
	magicOpmphm.size = SIZE_MAX;
}

/**
 * @brief Magic OpmphmPredictor initializer.
 *
 * @param magicNumber to detect arbitrary byte-swaps
 */
static void initMagicOpmphmPredictor (const uintptr_t magicNumber)
{
	magicOpmphmPredictor.history = UINT16_MAX;
	magicOpmphmPredictor.patternTable = (void *) magicNumber;
	magicOpmphmPredictor.size = SIZE_MAX / 2;
	magicOpmphmPredictor.lookupCount = 0;
	magicOpmphmPredictor.ksSize = SIZE_MAX;
}
#endif

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

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
/**
 * @brief Verify the magic Opmphm.
 *
 * @param opmphm to verify
 *
 * @retval 0 if magic Opmphm is consistent
 * @retval -1 if magic Opmphm is inconsistent
 */
static int verifyMagicOpmphm (Opmphm * opmphm)
{
	if (!opmphm) return -1;
	return memcmp (opmphm, &magicOpmphm, sizeof (Opmphm));
}

/**
 * @brief Verify the magic OpmphmPredictor.
 *
 * @param opmphmPredictor to verify
 *
 * @retval 0 if magic OpmphmPredictor is consistent
 * @retval -1 if magic OpmphmPredictor is inconsistent
 */
static int verifyMagicOpmphmPredictor (OpmphmPredictor * opmphmPredictor)
{
	if (!opmphmPredictor) return -1;
	return memcmp (opmphmPredictor, &magicOpmphmPredictor, sizeof (OpmphmPredictor));
}
#endif

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

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	Opmphm * opmphm = (Opmphm *) (mappedRegion + OFFSET_MAGIC_OPMPHM);
	OpmphmPredictor * opmphmPredictor = (OpmphmPredictor *) (mappedRegion + OFFSET_MAGIC_OPMPHMPREDICTOR);
	if ((verifyMagicOpmphm (opmphm) != 0) || (verifyMagicOpmphmPredictor (opmphmPredictor) != 0))
	{
		return -1;
	}
#endif

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
static int verifyChecksum (char * mappedRegion, MmapHeader * mmapHeader, PluginMode mode)
{
	// if file was written without checksum, we skip the check
	if (!test_bit (mmapHeader->formatFlags, MMAP_FLAG_CHECKSUM)) return 0;

	uint32_t checksum = crc32 (0L, Z_NULL, 0);
	checksum = crc32 (checksum, (const unsigned char *) (mappedRegion + SIZEOF_MMAPHEADER), mmapHeader->cksumSize);

	if (checksum != mmapHeader->checksum)
	{
		ELEKTRA_MMAP_LOG_WARNING ("old checksum: %ul", mmapHeader->checksum);
		ELEKTRA_MMAP_LOG_WARNING ("new checksum: %ul", checksum);
		return -1;
	}
	return 0;
}
#endif

/**
 * Calculate the data block size of all keys within a key set.
 * @param mmapMetaData the structure where to add the allocation size to
 * @param keySet the key set which contains all the keys to use for calculation
 * @param dynArray used for the `dynArrayFindOrInsert` function
 * @return the sum of all the data block sizes
 */
static size_t calculateDataBlockSize (MmapMetaData * mmapMetaData, const KeySet * keySet, DynArray * dynArray)
{
	Key * cur;
	size_t dataBlocksSize = 0;
	for (elektraCursor it = 0; it < ksGetSize (keySet); ++it)
	{
		cur = ksAtCursor (keySet, it);
		dataBlocksSize += (cur->keySize + cur->keyUSize + cur->dataSize);

		if (cur->meta && cur->meta->size > 0)
		{
			++mmapMetaData->numKeySets;

			Key * curMeta;
			for (elektraCursor itMeta = 0; itMeta < ksGetSize (cur->meta); ++itMeta)
			{
				curMeta = ksAtCursor (cur->meta, itMeta);
				if (ELEKTRA_PLUGIN_FUNCTION (dynArrayFindOrInsert) (curMeta, dynArray) == 0)
				{
					// key was just inserted
					dataBlocksSize += (curMeta->keySize + curMeta->keyUSize + curMeta->dataSize);
				}
			}
			mmapMetaData->ksAlloc += (cur->meta->alloc);
		}
	}
	return dataBlocksSize;
}
/**
 * @brief Calculates the size, in bytes, needed to store the KeySet in a mmap region.
 *
 * Iterates over the KeySet and calculates the complete size in bytes, needed to store the KeySet
 * within a mapped region. The size includes all mmap meta-information, magic KeySet and Key for
 * consistency checks, KeySets, meta-KeySets, Keys, meta-Keys, Key names and values.
 * Copied meta-Keys are counted once for deduplication. If needed, padding is added to align the
 * MmapFooter properly at the end of the mapping. When the plugin is in a global position,
 * acting as a cache, the calculated size includes the global KeySet.
 *
 * The complete size and some other meta-information are stored in the MmapHeader and MmapMetaData.
 * The DynArray stores the unique meta-Key pointers needed for deduplication.
 *
 * @param mmapHeader to store the allocation size
 * @param mmapMetaData to store the number of KeySets and Keys
 * @param returned the KeySet that should be stored
 * @param global the global KeySet
 * @param dynArray to store meta-key pointers for deduplication
 */
static void calculateMmapDataSize (MmapHeader * mmapHeader, MmapMetaData * mmapMetaData, KeySet * returned, KeySet * global,
				   DynArray * dynArray)
{
	size_t dataBlocksSize; // sum of keyName and keyValue sizes
	mmapMetaData->numKeys = 0;
	mmapMetaData->numKeySets = 3;		 // include the magic, global and main keyset
	mmapMetaData->ksAlloc = returned->alloc; // sum of allocation sizes for all meta-keysets

	dataBlocksSize = calculateDataBlockSize (mmapMetaData, returned, dynArray);

	if (global)
	{
		ELEKTRA_LOG_DEBUG ("calculate global keyset into size");
		mmapMetaData->ksAlloc += global->alloc;
		mmapMetaData->numKeys += global->size;

		dataBlocksSize += calculateDataBlockSize (mmapMetaData, global, dynArray);
	}
	mmapMetaData->numKeys += returned->size + dynArray->size + 1; // +1 for magic Key

	size_t opmphmSize = 0;
#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	// the OPMPHM structs are included regardless of existece in KeySet
	// s.t. the format stays the same for all KeySets
	opmphmSize = (sizeof (Opmphm) * 2) + (sizeof (OpmphmPredictor) * 2); // *2 for magic opmphm data
	Opmphm * opmphm = returned->opmphm;
	if (opmphm)
	{
		if (opmphm->rUniPar) dataBlocksSize += opmphm->rUniPar * sizeof (int32_t);
		dataBlocksSize += opmphm->size;
	}
	OpmphmPredictor * opmphmPredictor = returned->opmphmPredictor;
	if (opmphmPredictor)
	{
		dataBlocksSize += opmphmPredictor->size * sizeof (uint8_t);
	}
	ELEKTRA_LOG_DEBUG ("OPMPHM enabled, size: %zu", opmphmSize);
#endif

	size_t keyArraySize = mmapMetaData->numKeys * SIZEOF_KEY;
	mmapHeader->allocSize = (SIZEOF_MMAPMETADATA * 2) + opmphmSize + (SIZEOF_KEYSET * mmapMetaData->numKeySets) + keyArraySize +
				dataBlocksSize + (mmapMetaData->ksAlloc * SIZEOF_KEY_PTR);
	mmapHeader->cksumSize = mmapHeader->allocSize; // cksumSize now contains size of all critical data

	size_t padding = sizeof (uint64_t) - (mmapHeader->allocSize % sizeof (uint64_t)); // alignment for MMAP Footer at end of mapping
	mmapHeader->allocSize += SIZEOF_MMAPHEADER + padding + SIZEOF_MMAPFOOTER;

	mmapMetaData->numKeys--;    // don't include magic Key
	mmapMetaData->numKeySets--; // don't include magic KeySet
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
	if (dynArray->size == 0)
	{
		return;
	}

	// allocate space in DynArray to remember the addresses of mapped meta-keys
	dynArray->mappedKeyArray = elektraCalloc (dynArray->size * sizeof (Key *));

	// write the meta keys into place
	for (size_t i = 0; i < dynArray->size; ++i)
	{
		Key * curMeta = dynArray->keyArray[i];	      // old key location
		Key * mmapMetaKey = (Key *) mmapAddr->keyPtr; // new key location
		*mmapMetaKey = *curMeta;
		mmapAddr->keyPtr += SIZEOF_KEY;

		// move Key name
		if (curMeta->key)
		{
			memcpy (mmapAddr->dataPtr, curMeta->key, curMeta->keySize);
			mmapMetaKey->key = mmapAddr->dataPtr - mmapAddr->mmapAddrInt;
			mmapAddr->dataPtr += curMeta->keySize;
			mmapMetaKey->flags |= KEY_FLAG_MMAP_KEY;
		}

		// move Key unescaped name
		if (curMeta->ukey)
		{
			memcpy (mmapAddr->dataPtr, curMeta->ukey, curMeta->keyUSize);
			mmapMetaKey->ukey = mmapAddr->dataPtr - mmapAddr->mmapAddrInt;
			mmapAddr->dataPtr += curMeta->keyUSize;
			mmapMetaKey->flags |= KEY_FLAG_MMAP_KEY;
		}

		// move Key value
		if (curMeta->data.v)
		{
			memcpy (mmapAddr->dataPtr, curMeta->data.v, curMeta->dataSize);
			mmapMetaKey->data.v = mmapAddr->dataPtr - mmapAddr->mmapAddrInt;
			mmapAddr->dataPtr += curMeta->dataSize;
			mmapMetaKey->flags |= KEY_FLAG_MMAP_DATA;
		}

		// move Key itself
		mmapMetaKey->flags |= KEY_FLAG_MMAP_STRUCT;
		mmapMetaKey->meta = 0;
		mmapMetaKey->refs = 0;

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
	if (!key->meta || !(key->meta->size > 0)) return 0;

	KeySet * newMeta = (KeySet *) mmapAddr->metaKsPtr;
	mmapAddr->metaKsPtr += SIZEOF_KEYSET;

	newMeta->flags = key->meta->flags | KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
	newMeta->array = (Key **) mmapAddr->metaKsArrayPtr;
	mmapAddr->metaKsArrayPtr += SIZEOF_KEY_PTR * key->meta->alloc;

	size_t metaKeyIndex = 0;
	Key * mappedMetaKey = 0;
	const Key * metaKey;


	KeySet * metaKeys = keyMeta (key);
	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		metaKey = ksAtCursor (metaKeys, it);
		// get address of mapped key and store it in the new array
		mappedMetaKey = dynArray->mappedKeyArray[ELEKTRA_PLUGIN_FUNCTION (dynArrayFind) ((Key *) metaKey, dynArray)];
		newMeta->array[metaKeyIndex] = (Key *) ((char *) mappedMetaKey - mmapAddr->mmapAddrInt);
		if (mappedMetaKey->refs < UINT16_MAX - 1)
		{
			++(mappedMetaKey->refs);
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
static void writeKeys (KeySet * keySet, MmapAddr * mmapAddr, DynArray * dynArray, PluginMode mode)
{
	Key * cur;
	size_t keyIndex = 0;

	Key ** ksArray = 0;
	if (test_bit (mode, MODE_STORAGE))
	{
		ksArray = (Key **) mmapAddr->ksArrayPtr;
	}
	else
	{
		ksArray = (Key **) mmapAddr->globalKsArrayPtr;
	}

	for (elektraCursor it = 0; it < ksGetSize (keySet); ++it)
	{
		cur = ksAtCursor (keySet, it);
		Key * mmapKey = (Key *) mmapAddr->keyPtr; // new key location
		mmapAddr->keyPtr += SIZEOF_KEY;
		*mmapKey = *cur;

		// move Key name
		if (cur->key)
		{
			memcpy (mmapAddr->dataPtr, cur->key, cur->keySize);
			mmapKey->key = mmapAddr->dataPtr - mmapAddr->mmapAddrInt;
			mmapAddr->dataPtr += cur->keySize;
			mmapKey->flags |= KEY_FLAG_MMAP_KEY;
		}

		// move Key unescaped name
		if (cur->ukey)
		{
			memcpy (mmapAddr->dataPtr, cur->ukey, cur->keyUSize);
			mmapKey->ukey = mmapAddr->dataPtr - mmapAddr->mmapAddrInt;
			mmapAddr->dataPtr += cur->keyUSize;
			mmapKey->flags |= KEY_FLAG_MMAP_KEY;
		}

		// move Key value
		if (cur->data.v)
		{
			memcpy (mmapAddr->dataPtr, cur->data.v, cur->dataSize);
			mmapKey->data.v = mmapAddr->dataPtr - mmapAddr->mmapAddrInt;
			mmapAddr->dataPtr += cur->dataSize;
			mmapKey->flags |= KEY_FLAG_MMAP_DATA;
		}
		else
		{
			mmapKey->data.v = 0;
			mmapKey->dataSize = 0;
		}

		// write the meta KeySet
		mmapKey->meta = writeMetaKeySet (cur, mmapAddr, dynArray);

		// move Key itself
		mmapKey->flags |= KEY_FLAG_MMAP_STRUCT;
		mmapKey->refs = 1;

		// write the relative Key pointer into the KeySet array
		ksArray[keyIndex] = (Key *) ((char *) mmapKey - mmapAddr->mmapAddrInt);

		++keyIndex;
	}
}

static void printMmapAddr (MmapAddr * mmapAddr ELEKTRA_UNUSED)
{
	ELEKTRA_LOG_DEBUG ("globalKsPtr: \t\t %p", (void *) mmapAddr->globalKsPtr);
	ELEKTRA_LOG_DEBUG ("ksPtr: \t\t %p", (void *) mmapAddr->ksPtr);
	ELEKTRA_LOG_DEBUG ("metaKsPtr: \t\t %p", (void *) mmapAddr->metaKsPtr);
	ELEKTRA_LOG_DEBUG ("globalKsArrayPtr: \t %p", (void *) mmapAddr->globalKsArrayPtr);
	ELEKTRA_LOG_DEBUG ("ksArrayPtr: \t\t %p", (void *) mmapAddr->ksArrayPtr);
	ELEKTRA_LOG_DEBUG ("metaKsArrayPtr: \t %p", (void *) mmapAddr->metaKsArrayPtr);
	ELEKTRA_LOG_DEBUG ("keyPtr: \t\t %p", (void *) mmapAddr->keyPtr);
	ELEKTRA_LOG_DEBUG ("dataPtr: \t\t %p", (void *) mmapAddr->dataPtr);
	ELEKTRA_LOG_DEBUG ("mmapAddrInt: \t\t %lu", mmapAddr->mmapAddrInt);
}

static void printMmapMetaData (MmapMetaData * mmapMetaData ELEKTRA_UNUSED)
{
	ELEKTRA_LOG_DEBUG ("numKeySets: \t %lu", mmapMetaData->numKeySets);
	ELEKTRA_LOG_DEBUG ("ksAlloc: \t %lu", mmapMetaData->ksAlloc);
	ELEKTRA_LOG_DEBUG ("numKeys: \t %lu", mmapMetaData->numKeys);
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
 * @param global global keyset
 * @param mmapHeader containing the size and checksum of the mapped region
 * @param mmapMetaData containing meta-information of the mapped region
 * @param mmapFooter containing a magic number for consistency checks
 * @param dynArray containing deduplicated pointers to meta-keys
 * @param mode the current plugin mode
 *
 * @retval 0 on success
 * @retval -1 if msync() failed
 */
static int copyKeySetToMmap (char * const dest, KeySet * keySet, KeySet * global, MmapHeader * mmapHeader, MmapMetaData * mmapMetaData,
			     MmapFooter * mmapFooter, DynArray * dynArray, PluginMode mode)
{
	writeMagicData (dest);

	size_t globalAlloc = 0;
	if (global) globalAlloc = global->alloc;

	MmapAddr mmapAddr = { .globalKsPtr = (KeySet *) (dest + OFFSET_GLOBAL_KEYSET),
			      .ksPtr = (KeySet *) (dest + OFFSET_KEYSET),
			      .metaKsPtr = (char *) mmapAddr.ksPtr + SIZEOF_KEYSET,
			      .globalKsArrayPtr = (dest + OFFSET_GLOBAL_KEYSET) + (SIZEOF_KEYSET * mmapMetaData->numKeySets),
			      .ksArrayPtr = mmapAddr.globalKsArrayPtr + (SIZEOF_KEY_PTR * globalAlloc),
			      .metaKsArrayPtr = mmapAddr.ksArrayPtr + (SIZEOF_KEY_PTR * keySet->alloc),
			      .keyPtr = mmapAddr.globalKsArrayPtr + (SIZEOF_KEY_PTR * mmapMetaData->ksAlloc),
			      .dataPtr = mmapAddr.keyPtr + (SIZEOF_KEY * mmapMetaData->numKeys),
			      .mmapAddrInt = (uintptr_t) dest };

	printMmapAddr (&mmapAddr);
	printMmapMetaData (mmapMetaData);

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	// We FIRST write the opmphm data. If this is changed, you'll run into alignment problems.
	// set OPMPHM flag, so file is not readable by builds without OPMPHM
	set_bit (mmapHeader->formatFlags, MMAP_FLAG_OPMPHM);
	if (keySet->opmphm)
	{
		mmapAddr.ksPtr->opmphm = (Opmphm *) (dest + OFFSET_OPMPHM);
		memcpy (mmapAddr.ksPtr->opmphm, keySet->opmphm, sizeof (Opmphm));
		mmapAddr.ksPtr->opmphm->flags = OPMPHM_FLAG_MMAP_STRUCT;
		if (keySet->opmphm->rUniPar)
		{
			mmapAddr.ksPtr->opmphm->hashFunctionSeeds = (int32_t *) mmapAddr.dataPtr;
			memcpy (mmapAddr.ksPtr->opmphm->hashFunctionSeeds, keySet->opmphm->hashFunctionSeeds,
				keySet->opmphm->rUniPar * sizeof (int32_t));
			set_bit (mmapAddr.ksPtr->opmphm->flags, OPMPHM_FLAG_MMAP_HASHFUNCTIONSEEDS);
			mmapAddr.dataPtr += keySet->opmphm->rUniPar * sizeof (int32_t);
			mmapAddr.ksPtr->opmphm->hashFunctionSeeds =
				(int32_t *) (((char *) mmapAddr.ksPtr->opmphm->hashFunctionSeeds) - mmapAddr.mmapAddrInt);
		}
		if (keySet->opmphm->size)
		{
			mmapAddr.ksPtr->opmphm->graph = (uint32_t *) mmapAddr.dataPtr;
			memcpy (mmapAddr.ksPtr->opmphm->graph, keySet->opmphm->graph, keySet->opmphm->size);
			set_bit (mmapAddr.ksPtr->opmphm->flags, OPMPHM_FLAG_MMAP_GRAPH);
			mmapAddr.dataPtr += keySet->opmphm->size;
			mmapAddr.ksPtr->opmphm->graph = (uint32_t *) (((char *) mmapAddr.ksPtr->opmphm->graph) - mmapAddr.mmapAddrInt);
		}
		mmapAddr.ksPtr->opmphm = (Opmphm *) (((char *) mmapAddr.ksPtr->opmphm) - mmapAddr.mmapAddrInt);
	}
	if (keySet->opmphmPredictor)
	{
		ELEKTRA_LOG_DEBUG ("OPMPHM: will store predictor");
		mmapAddr.ksPtr->opmphmPredictor = (OpmphmPredictor *) (dest + OFFSET_OPMPHMPREDICTOR);
		memcpy (mmapAddr.ksPtr->opmphmPredictor, keySet->opmphmPredictor, sizeof (OpmphmPredictor));
		mmapAddr.ksPtr->opmphmPredictor->patternTable = (uint8_t *) mmapAddr.dataPtr;
		memcpy (mmapAddr.ksPtr->opmphmPredictor->patternTable, keySet->opmphmPredictor->patternTable,
			keySet->opmphmPredictor->size * sizeof (uint8_t));
		mmapAddr.ksPtr->opmphmPredictor->flags = OPMPHM_PREDICTOR_FLAG_MMAP_STRUCT | OPMPHM_PREDICTOR_FLAG_MMAP_PATTERNTABLE;
		mmapAddr.dataPtr += keySet->opmphmPredictor->size * sizeof (uint8_t);
		mmapAddr.ksPtr->opmphmPredictor->patternTable =
			(uint8_t *) (((char *) mmapAddr.ksPtr->opmphmPredictor->patternTable) - mmapAddr.mmapAddrInt);
		mmapAddr.ksPtr->opmphmPredictor = (OpmphmPredictor *) (((char *) mmapAddr.ksPtr->opmphmPredictor) - mmapAddr.mmapAddrInt);
	}
#endif

	// first write the meta keys into place
	writeMetaKeys (&mmapAddr, dynArray);

	if (global)
	{
		ELEKTRA_LOG_DEBUG ("writing GLOBAL KEYSET");
		if (global->size != 0) writeKeys (global, &mmapAddr, dynArray, MODE_GLOBALCACHE);

		set_bit (mmapHeader->formatFlags, MMAP_FLAG_TIMESTAMPS);
		mmapAddr.globalKsPtr->flags = global->flags | KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
		mmapAddr.globalKsPtr->array = (Key **) mmapAddr.globalKsArrayPtr;
		mmapAddr.globalKsPtr->array[global->size] = 0;
		mmapAddr.globalKsPtr->array = (Key **) (mmapAddr.globalKsArrayPtr - mmapAddr.mmapAddrInt);
		mmapAddr.globalKsPtr->alloc = global->alloc;
		mmapAddr.globalKsPtr->size = global->size;
	}

	if (keySet->size != 0)
	{
		// now write Keys including meta KeySets
		writeKeys (keySet, &mmapAddr, dynArray, MODE_STORAGE);
	}

	mmapAddr.ksPtr->flags = keySet->flags | KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
	mmapAddr.ksPtr->array = (Key **) mmapAddr.ksArrayPtr;
	mmapAddr.ksPtr->array[keySet->size] = 0;
	mmapAddr.ksPtr->array = (Key **) (mmapAddr.ksArrayPtr - mmapAddr.mmapAddrInt);
	mmapAddr.ksPtr->alloc = keySet->alloc;
	mmapAddr.ksPtr->size = keySet->size;

	memcpy ((dest + OFFSET_MMAPMETADATA), mmapMetaData, SIZEOF_MMAPMETADATA);
#ifdef ELEKTRA_MMAP_CHECKSUM
	uint32_t checksum = crc32 (0L, Z_NULL, 0);
	checksum = crc32 (checksum, (const unsigned char *) (dest + SIZEOF_MMAPHEADER), mmapHeader->cksumSize);
	mmapHeader->checksum = checksum;
#endif
	memcpy (dest, mmapHeader, SIZEOF_MMAPHEADER);

	// force footer to be written last, as a consistency check
	if (!test_bit (mode, MODE_NONREGULAR_FILE) && msync ((void *) dest, mmapHeader->allocSize, MS_SYNC) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not msync");
		return -1;
	}

	memcpy ((dest + mmapHeader->allocSize - SIZEOF_MMAPFOOTER), mmapFooter, SIZEOF_MMAPFOOTER);

	return 0;
}

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
/**
 * @brief Deletes the OPMPHM.
 *
 * Clears and frees all memory in Opmphm.
 *
 * @param opmphm the OPMPHM
 */
static void mmapOpmphmDel (Opmphm * opmphm)
{
	ELEKTRA_NOT_NULL (opmphm);
	if (opmphm && opmphm->size && !test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_GRAPH))
	{
		elektraFree (opmphm->graph);
	}
	if (opmphm->rUniPar && !test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_HASHFUNCTIONSEEDS))
	{
		elektraFree (opmphm->hashFunctionSeeds);
	}
	if (!test_bit (opmphm->flags, OPMPHM_FLAG_MMAP_STRUCT)) elektraFree (opmphm);
}

/**
 * @brief Deletes the OpmphmPredictor.
 *
 * Clears and frees all memory in OpmphmPredictor.
 *
 * @param op the OpmphmPredictor
 */
static void mmapOpmphmPredictorDel (OpmphmPredictor * op)
{
	ELEKTRA_NOT_NULL (op);
	if (!test_bit (op->flags, OPMPHM_PREDICTOR_FLAG_MMAP_PATTERNTABLE)) elektraFree (op->patternTable);
	if (!test_bit (op->flags, OPMPHM_PREDICTOR_FLAG_MMAP_STRUCT)) elektraFree (op);
}
#endif

/**
 * @brief Replaces contents of dest with the keyset from src.
 *
 * The keyset members of dest are replaced with data from src.
 *
 * @param dest destination keyset
 * @param src source keyset
 */
static void mmapFastReplace (KeySet * dest, KeySet * src)
{
	ksClose (dest);
	dest->array = src->array;
	dest->size = src->size;
	dest->alloc = src->alloc;
	// to be able to free() the returned KeySet, just set the array flag here
	dest->flags = KS_FLAG_MMAP_ARRAY;
	// we intentionally do not change the KeySet->opmphm here!
	// also do not change the reference counter on purpose,
	// we only want to change the contents
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
static void mmapToKeySet (Plugin * handle, char * mappedRegion, KeySet * returned, PluginMode mode)
{
	KeySet * keySet = (KeySet *) (mappedRegion + OFFSET_KEYSET);
	mmapFastReplace (returned, keySet);

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	if (keySet->opmphm)
	{
		if (returned->opmphm) mmapOpmphmDel (returned->opmphm);
		returned->opmphm = keySet->opmphm;
	}
	if (keySet->opmphmPredictor)
	{
		if (returned->opmphmPredictor) mmapOpmphmPredictorDel (returned->opmphmPredictor);
		returned->opmphmPredictor = keySet->opmphmPredictor;
	}
#endif

	if (test_bit (mode, MODE_GLOBALCACHE))
	{
		KeySet * global = elektraPluginGetGlobalKeySet (handle);
		KeySet * mmapTimeStamps = (KeySet *) (mappedRegion + OFFSET_GLOBAL_KEYSET);

		if (ksGetSize (global) == 0)
		{
			// fast replace KeySet, if Global KeySet was empty
			mmapFastReplace (global, mmapTimeStamps);
		}
		else
		{
			// do not overwrite, but append Keys to existing Global KeySet
			ksAppend (global, mmapTimeStamps);
		}
	}
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

	char * ksPtr = (dest + OFFSET_GLOBAL_KEYSET);
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
				ks->array[j] = (Key *) ((char *) (ks->array[j]) + destInt);
			}
		}
	}

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	KeySet * returnedKs = (KeySet *) (dest + OFFSET_KEYSET);
	if (returnedKs->opmphm)
	{
		returnedKs->opmphm = (Opmphm *) (dest + OFFSET_OPMPHM);
		if (returnedKs->opmphm->hashFunctionSeeds)
		{
			returnedKs->opmphm->hashFunctionSeeds = (int32_t *) (((char *) returnedKs->opmphm->hashFunctionSeeds) + destInt);
		}
		if (returnedKs->opmphm->graph)
		{
			returnedKs->opmphm->graph = (uint32_t *) (((char *) returnedKs->opmphm->graph) + destInt);
		}
	}
	if (returnedKs->opmphmPredictor)
	{
		returnedKs->opmphmPredictor = (OpmphmPredictor *) (dest + OFFSET_OPMPHMPREDICTOR);
		if (returnedKs->opmphmPredictor->patternTable)
		{
			returnedKs->opmphmPredictor->patternTable =
				(uint8_t *) (((char *) returnedKs->opmphmPredictor->patternTable) + destInt);
		}
	}
#endif

	for (size_t i = 0; i < mmapMetaData->numKeys; ++i)
	{
		Key * key = (Key *) keyPtr;
		keyPtr += SIZEOF_KEY;
		if (key->data.c)
		{
			key->data.c = key->data.c + destInt;
		}
		if (key->key)
		{
			key->key = key->key + destInt;
		}
		if (key->ukey)
		{
			key->ukey = key->ukey + destInt;
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
int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic

	// sanity checks first, return error in non-debug builds to avoid undefined behavior
	if (sizeof (MmapHeader) != STATIC_SIZEOF_MMAPHEADER) goto error;
	if (offsetof (MmapHeader, mmapMagicNumber) != STATIC_HEADER_OFFSETOF_MAGICNUMBER) goto error;
	if (offsetof (MmapHeader, allocSize) != STATIC_HEADER_OFFSETOF_ALLOCSIZE) goto error;
	if (offsetof (MmapHeader, cksumSize) != STATIC_HEADER_OFFSETOF_CHECKSUMSIZE) goto error;
	if (offsetof (MmapHeader, checksum) != STATIC_HEADER_OFFSETOF_CHECKSUM) goto error;
	if (offsetof (MmapHeader, formatFlags) != STATIC_HEADER_OFFSETOF_FORMATFLAGS) goto error;
	if (offsetof (MmapHeader, formatVersion) != STATIC_HEADER_OFFSETOF_FORMATVERSION) goto error;
	if (offsetof (MmapHeader, reservedA) != STATIC_HEADER_OFFSETOF_RESERVED_A) goto error;
	if (offsetof (MmapHeader, reservedB) != STATIC_HEADER_OFFSETOF_RESERVED_B) goto error;
	if (sizeof (MmapFooter) != STATIC_SIZEOF_MMAPFOOTER) goto error;
	if (offsetof (MmapFooter, mmapMagicNumber) != STATIC_FOOTER_OFFSETOF_MAGICNUMBER) goto error;

	// initialize magic data
	const uintptr_t magicNumber = generateMagicNumber ();
	if (magicKeySet.array == 0) initMagicKeySet (magicNumber);
	if (magicKey.data.v == 0) initMagicKey (magicNumber);
	if (magicMmapMetaData.numKeys == 0) initMagicMmapMetaData ();

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	if (magicOpmphm.size == 0) initMagicOpmphm (magicNumber);
	if (magicOpmphmPredictor.ksSize == 0) initMagicOpmphmPredictor (magicNumber);
#endif

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;

error:
	ELEKTRA_SET_INTERNAL_ERROR (errorKey,
				    "The MmapHeader or Mmap data structure was changed in a way that breaks compatibility with existing "
				    "mmapstorage files. This can lead to undefined behavior so mmapstorage is aborting here.");
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

/**
 * @brief Cleans up the plugin data and unlinks all mapped files.
 *
 * @param handle The plugin handle.
 * @param errorKey Unused.
 *
 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS always.
 */
int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
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
int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
{
	// get all keys
	int errnosave = errno;
	PluginMode mode = MODE_STORAGE;

	if (elektraPluginGetGlobalKeySet (handle) != 0)
	{
		ELEKTRA_LOG_DEBUG ("mmapstorage global position called");
		mode = MODE_GLOBALCACHE;
	}

	Key * root = keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME, KEY_END);
	if (keyCmp (root, parentKey) == 0 || keyIsBelow (root, parentKey) == 1)
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
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);

	if (elektraStrCmp (keyString (parentKey), STDIN_FILENAME) == 0)
	{
		fd = fileno (stdin);
		ELEKTRA_LOG_DEBUG ("MODE_NONREGULAR_FILE");
		set_bit (mode, MODE_NONREGULAR_FILE);
	}
	else if ((fd = openFile (parentKey, O_RDONLY, 0, mode)) == -1)
	{
		goto error;
	}

	struct stat sbuf;
	if (fstatFile (fd, &sbuf, parentKey, mode) != 1)
	{
		goto error;
	}

	if (test_bit (mode, MODE_NONREGULAR_FILE))
	{
		// non regular file not mmap compatible, copy to temp file
		ELEKTRA_LOG_DEBUG ("copy nonregular file");
		if ((fd = copyToAnonymousTempfile (fd, &sbuf, parentKey, mode)) < 0)
		{
			goto error;
		}
		clear_bit (mode, MODE_NONREGULAR_FILE);
	}

	if (sbuf.st_size == 0)
	{
		// empty mmap file
		if (close (fd) != 0)
		{
			ELEKTRA_MMAP_LOG_WARNING ("could not close");
			goto error;
		}
		keySetString (parentKey, keyString (initialParent));
		if (initialParent) keyDel (initialParent);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (sbuf.st_size < 0 || (size_t) sbuf.st_size < ELEKTRA_MMAP_MINSIZE)
	{
		// file is smaller than minimum size
		goto error;
	}

	mappedRegion = mmapFile ((void *) 0, fd, sbuf.st_size, MAP_PRIVATE, parentKey, mode);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_MMAP_LOG_WARNING ("mappedRegion == MAP_FAILED");
		goto error;
	}

	if (verifyMagicData (mappedRegion) != 0)
	{
		// magic data could not be read properly, indicating unreadable format or different architecture
		ELEKTRA_MMAP_LOG_WARNING ("mmap magic data could not be read properly");
		goto error;
	}

	MmapHeader * mmapHeader;
	MmapMetaData * mmapMetaData;
	if (readHeader (mappedRegion, &mmapHeader, &mmapMetaData) == -1)
	{
		// config file was corrupt
		ELEKTRA_MMAP_LOG_WARNING ("could not read mmap information header");
		goto error;
	}

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	if (!test_bit (mmapHeader->formatFlags, MMAP_FLAG_OPMPHM))
	{
		ELEKTRA_MMAP_LOG_WARNING ("mmap file written without OPMPHM, but reading from a build with OPMPHM");
		goto error;
	}
#else
	if (test_bit (mmapHeader->formatFlags, MMAP_FLAG_OPMPHM))
	{
		ELEKTRA_MMAP_LOG_WARNING ("mmap file written with OPMPHM, but reading from a build without OPMPHM");
		goto error;
	}
#endif

	if (sbuf.st_size < 0 || (size_t) sbuf.st_size != mmapHeader->allocSize)
	{
		// config file size mismatch
		ELEKTRA_MMAP_LOG_WARNING ("mmap file size differs from metadata, file was altered");
		goto error;
	}

#ifdef ELEKTRA_MMAP_CHECKSUM
	if (verifyChecksum (mappedRegion, mmapHeader, mode) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("checksum failed");
		goto error;
	}
#endif

	if (!test_bit (mmapHeader->formatFlags, MMAP_FLAG_TIMESTAMPS) && mode == MODE_GLOBALCACHE)
	{
		ELEKTRA_MMAP_LOG_WARNING ("plugin in global cache mode, but file does not contain timestamps");
	}

	if (readFooter (mappedRegion, mmapHeader) == -1)
	{
		// config file was corrupt/truncated
		ELEKTRA_MMAP_LOG_WARNING ("could not read mmap information footer: file was altered");
		goto error;
	}

	updatePointers (mmapMetaData, mappedRegion);
	mmapToKeySet (handle, mappedRegion, ks, mode);

	if (close (fd) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not close");
		goto error;
	}

	keySetString (parentKey, keyString (initialParent));
	if (initialParent) keyDel (initialParent);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;

error:
	if (errno != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("strerror: %s", strerror (errno));
		ELEKTRA_SET_ERROR_GET (parentKey);
	}

	if ((mappedRegion != MAP_FAILED) && (munmap (mappedRegion, sbuf.st_size) != 0))
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not munmap");
	}
	if ((fd != -1) && close (fd) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not close");
	}

	keySetString (parentKey, keyString (initialParent));
	if (initialParent) keyDel (initialParent);
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
int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
{
	// set all keys
	KeySet * global = 0;
	PluginMode mode = MODE_STORAGE;

	if ((global = elektraPluginGetGlobalKeySet (handle)) != 0)
	{
		ELEKTRA_LOG_DEBUG ("mmapstorage global position called");
		mode = MODE_GLOBALCACHE;
	}

	int errnosave = errno;
	int fd = -1;
	char * mappedRegion = MAP_FAILED;
	DynArray * dynArray = 0;
	Key * initialParent = keyDup (parentKey, KEY_CP_ALL);

	if (elektraStrCmp (keyString (parentKey), STDOUT_FILENAME) == 0)
	{
		// keySetString (parentKey, keyString (initialParent));
		fd = fileno (stdout);
		ELEKTRA_LOG_DEBUG ("MODE_NONREGULAR_FILE");
		set_bit (mode, MODE_NONREGULAR_FILE);
	}
	else
	{
		if (unlink (keyString (parentKey)) != 0 && errno != ENOENT)
		{
			ELEKTRA_MMAP_LOG_WARNING ("could not unlink");
			goto error;
		}

		if ((fd = openFile (parentKey, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR, mode)) == -1)
		{
			goto error;
		}
	}

	struct stat sbuf;
	if (fstatFile (fd, &sbuf, parentKey, mode) != 1)
	{
		goto error;
	}

	dynArray = ELEKTRA_PLUGIN_FUNCTION (dynArrayNew) ();

	MmapHeader mmapHeader;
	MmapMetaData mmapMetaData;
	initHeader (&mmapHeader);
	initMetaData (&mmapMetaData);
	calculateMmapDataSize (&mmapHeader, &mmapMetaData, ks, global, dynArray);
	ELEKTRA_LOG_DEBUG ("mmapsize: %" PRIu64, mmapHeader.allocSize);

	if (!test_bit (mode, MODE_NONREGULAR_FILE) && truncateFile (fd, mmapHeader.allocSize, parentKey, mode) != 1)
	{
		goto error;
	}

	mappedRegion = mmapFile ((void *) 0, fd, mmapHeader.allocSize, MAP_SHARED, parentKey, mode);
	ELEKTRA_LOG_DEBUG ("mappedRegion ptr: %p", (void *) mappedRegion);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_MMAP_LOG_WARNING ("mappedRegion == MAP_FAILED");
		goto error;
	}

	MmapFooter mmapFooter;
	initFooter (&mmapFooter);
	if (copyKeySetToMmap (mappedRegion, ks, global, &mmapHeader, &mmapMetaData, &mmapFooter, dynArray, mode) != 0)
	{
		goto error;
	}

	if (test_bit (mode, MODE_NONREGULAR_FILE))
	{
		ssize_t writtenBytes = write (fd, mappedRegion, mmapHeader.allocSize);
		if (writtenBytes == -1)
		{
			ELEKTRA_MMAP_LOG_WARNING ("could not write buffer to file");
			goto error;
		}
		elektraFree (mappedRegion);
		mappedRegion = MAP_FAILED;
	}

	if (!test_bit (mode, MODE_NONREGULAR_FILE) && msync ((void *) mappedRegion, mmapHeader.allocSize, MS_SYNC) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not msync");
		goto error;
	}

	if (!test_bit (mode, MODE_NONREGULAR_FILE) && munmap (mappedRegion, mmapHeader.allocSize) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not munmap");
		goto error;
	}

	if (close (fd) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not close");
		goto error;
	}

	ELEKTRA_PLUGIN_FUNCTION (dynArrayDelete) (dynArray);
	keySetString (parentKey, keyString (initialParent));
	if (initialParent) keyDel (initialParent);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;

error:
	if (errno != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("strerror: %s", strerror (errno));
		ELEKTRA_SET_ERROR_SET (parentKey);
	}

	if ((mappedRegion != MAP_FAILED && !test_bit (mode, MODE_NONREGULAR_FILE)) && (munmap (mappedRegion, mmapHeader.allocSize) != 0))
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not munmap");
	}

	if (mappedRegion != MAP_FAILED && test_bit (mode, MODE_NONREGULAR_FILE))
	{
		elektraFree (mappedRegion);
	}

	if ((fd != -1) && close (fd) != 0)
	{
		ELEKTRA_MMAP_LOG_WARNING ("could not close");
	}

	keySetString (parentKey, keyString (initialParent));
	if (initialParent) keyDel (initialParent);
	ELEKTRA_PLUGIN_FUNCTION (dynArrayDelete) (dynArray);

	errno = errnosave;
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport (ELEKTRA_PLUGIN_NAME,
		ELEKTRA_PLUGIN_OPEN,	&ELEKTRA_PLUGIN_FUNCTION(open),
		ELEKTRA_PLUGIN_CLOSE,	&ELEKTRA_PLUGIN_FUNCTION(close),
		ELEKTRA_PLUGIN_GET,	&ELEKTRA_PLUGIN_FUNCTION(get),
		ELEKTRA_PLUGIN_SET,	&ELEKTRA_PLUGIN_FUNCTION(set),
		ELEKTRA_PLUGIN_END);
}
