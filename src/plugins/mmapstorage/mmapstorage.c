/**
 * @file
 *
 * @brief Source for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "mmapstorage.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbprivate.h>

//#include <fcntl.h>	// open()
#include <assert.h>
#include <errno.h>
#include <limits.h>    // SSIZE_MAX
#include <stdio.h>     // fopen(), fileno()
#include <stdlib.h>    // strtol()
#include <sys/mman.h>  // mmap()
#include <sys/stat.h>  // stat()
#include <sys/types.h> // ftruncate ()
#include <unistd.h>    // close(), ftruncate()

//#include <zlib.h> // crc32()

/* -- Macros & Definitions -------------------------------------------------------------------------------------------------------------- */

#define SIZEOF_KEY (sizeof (Key))
#define SIZEOF_KEY_PTR (sizeof (Key *))
#define SIZEOF_KEYSET (sizeof (KeySet))
#define SIZEOF_KEYSET_PTR (sizeof (KeySet *))
#define SIZEOF_MMAPHEADER (sizeof (MmapHeader))
#define SIZEOF_MMAPFOOTER (sizeof (MmapFooter))
#define SIZEOF_ADDR_STRING (19) // format: 0xADDR -> ADDR in hex, for 64bit addr: 2 bytes (0x) + 16 bytes (ADDR) + 1 byte (ending null)

typedef enum _DestType { TYPE_MMAP = 0, TYPE_ALLOC } DestType;

/* -- File handling --------------------------------------------------------------------------------------------------------------------- */

static FILE * openFile (Key * parentKey, const char * mode)
{
	FILE * fp;
	ELEKTRA_LOG_DEBUG ("opening file %s", keyString (parentKey));

	if ((fp = fopen (keyString (parentKey), mode)) == 0)
	{
		ELEKTRA_LOG_WARNING ("error opening file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
	}
	return fp;
}

static int truncateFile (FILE * fp, size_t mmapsize, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG_DEBUG ("truncating file %s", keyString (parentKey));

	// TODO: does it matter whether we use truncate or ftruncate?
	int fd = fileno (fp);
	if ((ftruncate (fd, mmapsize)) == -1)
	{
		ELEKTRA_LOG_WARNING ("error truncating file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("mmapsize: %zu", mmapsize);
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
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
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
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
		ELEKTRA_LOG_WARNING ("error mapping file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("mmapSize: %zu", mmapSize);
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
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

#ifdef DEBUG
int findOrInsert (Key * key, DynArray * dynArray)
#else
static int findOrInsert (Key * key, DynArray * dynArray)
#endif
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

static void initFooter (MmapFooter * mmapFooter)
{
	mmapFooter->mmapMagicNumber = ELEKTRA_MAGIC_MMAP_NUMBER;
}

static int readHeader (FILE * fp, MmapHeader * mmapHeader)
{
	memset (mmapHeader, 0, SIZEOF_MMAPHEADER);
	fread (mmapHeader, SIZEOF_MMAPHEADER, (sizeof (char)), fp);

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

static void calculateDataSize (DestType destType, MmapHeader * mmapHeader, KeySet * returned, DynArray * dynArray)
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

	size_t keyArraySize = (returned->size) * SIZEOF_KEY;
	size_t keyPtrArraySize = (returned->alloc) * SIZEOF_KEY_PTR;

	size_t allocSize = SIZEOF_KEYSET + keyPtrArraySize + keyArraySize + dataBlocksSize + (metaKeySets * SIZEOF_KEYSET) +
			   (metaKsAlloc * SIZEOF_KEY_PTR) + (dynArray->size * SIZEOF_KEY);

	if (destType == TYPE_MMAP)
	{
		allocSize += SIZEOF_MMAPHEADER + SIZEOF_MMAPFOOTER;
	}
	mmapHeader->allocSize = allocSize;
	mmapHeader->numKeySets = 1 + metaKeySets;
	mmapHeader->ksAlloc = returned->alloc + metaKsAlloc;
	mmapHeader->numKeys = returned->size + dynArray->size;
	mmapHeader->dataSize = dataBlocksSize;
}

static void writeKeySet (DestType destType, MmapHeader * mmapHeader, KeySet * keySet, KeySet * dest, DynArray * dynArray)
{
	size_t mmapAddrInt = (size_t) mmapHeader->destAddr;

	KeySet * ksPtr = dest;
	char * metaKsPtr = (char *) ksPtr + SIZEOF_KEYSET; // meta keysets directly after the original keyset
	char * ksArrayPtr = (((char *) ksPtr) + (SIZEOF_KEYSET * mmapHeader->numKeySets));
	char * ksArrayPtrOrig = ksArrayPtr;
	char * metaKsArrayPtr = ksArrayPtr + (SIZEOF_KEY_PTR * keySet->alloc);
	char * keyPtr = (ksArrayPtr + (SIZEOF_KEY_PTR * mmapHeader->ksAlloc));
	char * dataPtr = ((char *) keyPtr + (SIZEOF_KEY * mmapHeader->numKeys));

	// allocate space in DynArray to remember the addresses of meta keys
	if (dynArray->size > 0)
	{
		dynArray->mappedKeyArray = elektraCalloc (dynArray->size * sizeof (Key *));
	}


	// first write the meta keys into place
	Key * mmapMetaKey;
	Key * curMeta;
	void * metaKeyNamePtr;
	void * metaKeyValuePtr;
	for (size_t i = 0; i < dynArray->size; ++i)
	{
		curMeta = dynArray->keyArray[i]; // old key location
		mmapMetaKey = (Key *) keyPtr;    // new key location
		keyPtr += SIZEOF_KEY;

		size_t keyNameSize = curMeta->keySize + curMeta->keyUSize;
		size_t keyValueSize = curMeta->dataSize;

		// move Key name
		if (curMeta->key)
		{
			memcpy (dataPtr, curMeta->key, keyNameSize);
			metaKeyNamePtr = dataPtr;
			dataPtr += keyNameSize;
		}
		else
		{
			metaKeyNamePtr = 0;
		}

		// move Key value
		if (curMeta->data.v)
		{
			memcpy (dataPtr, curMeta->data.v, keyValueSize);
			metaKeyValuePtr = (void *) dataPtr;
			dataPtr += keyValueSize;
		}
		else
		{
			metaKeyValuePtr = 0;
		}

		// move Key itself
		mmapMetaKey->flags = KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_KEY | KEY_FLAG_MMAP_DATA;
		if (destType == TYPE_MMAP && metaKeyNamePtr)
		{
			mmapMetaKey->key = ((char *) metaKeyNamePtr - mmapAddrInt);
		}
		else
		{
			mmapMetaKey->key = metaKeyNamePtr;
		}
		if (destType == TYPE_MMAP && metaKeyValuePtr)
		{
			mmapMetaKey->data.v = (void *) ((char *) metaKeyValuePtr - mmapAddrInt);
		}
		else
		{
			mmapMetaKey->data.v = metaKeyValuePtr;
		}
		mmapMetaKey->meta = 0;
		mmapMetaKey->ksReference = 0;
		mmapMetaKey->dataSize = curMeta->dataSize;
		mmapMetaKey->keySize = curMeta->keySize;
		mmapMetaKey->keyUSize = curMeta->keyUSize;

		dynArray->mappedKeyArray[i] = mmapMetaKey;
	}


	Key * cur;
	Key * mmapKey;
	void * keyNamePtr;
	void * keyValuePtr;
	size_t keyIndex = 0;
	ksRewind (keySet);
	while ((cur = ksNext (keySet)) != 0)
	{
		mmapKey = (Key *) keyPtr; // new key location
		keyPtr += SIZEOF_KEY;
		size_t keyNameSize = cur->keySize + cur->keyUSize;
		size_t keyValueSize = cur->dataSize;

		// move Key name
		if (cur->key)
		{
			memcpy (dataPtr, cur->key, keyNameSize);
			keyNamePtr = dataPtr;
			dataPtr += keyNameSize;
		}
		else
		{
			keyNamePtr = 0;
		}


		// move Key value
		if (cur->data.v)
		{
			memcpy (dataPtr, cur->data.v, keyValueSize);
			keyValuePtr = (void *) dataPtr;
			dataPtr += keyValueSize;
		}
		else
		{
			keyValuePtr = 0;
		}

		// write the meta KeySet
		KeySet * oldMeta = cur->meta;
		KeySet * newMeta = 0;
		if (oldMeta)
		{
			newMeta = (KeySet *) metaKsPtr;
			metaKsPtr += SIZEOF_KEYSET;

			newMeta->flags = KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
			newMeta->array = (Key **) metaKsArrayPtr;
			metaKsArrayPtr += SIZEOF_KEY_PTR * oldMeta->alloc;

			keyRewindMeta (cur);
			size_t metaKeyIndex = 0;
			Key * mappedMetaKey = 0;
			const Key * metaKey;
			while ((metaKey = keyNextMeta (cur)) != 0)
			{
				// get address of mapped key and store it in the new array
				mappedMetaKey = dynArray->mappedKeyArray[find ((Key *) metaKey, dynArray)];
				if (destType == TYPE_MMAP)
				{
					newMeta->array[metaKeyIndex] = (Key *) ((char *) mappedMetaKey - mmapAddrInt);
				}
				else
				{
					newMeta->array[metaKeyIndex] = mappedMetaKey;
				}
				if (mappedMetaKey->ksReference < SSIZE_MAX)
				{
					++(mappedMetaKey->ksReference);
				}
				++metaKeyIndex;
			}
			newMeta->array[oldMeta->size] = 0;
			if (destType == TYPE_MMAP)
			{
				newMeta->array = (Key **) ((char *) newMeta->array - mmapAddrInt);
			}
			newMeta->alloc = oldMeta->alloc;
			newMeta->size = oldMeta->size;
			newMeta->cursor = 0;
			newMeta->current = 0;
		}

		// move Key itself
		mmapKey->flags = KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_KEY | KEY_FLAG_MMAP_DATA;
		if (destType == TYPE_MMAP && keyNamePtr)
		{
			mmapKey->key = ((char *) keyNamePtr - mmapAddrInt);
		}
		else
		{
			mmapKey->key = keyNamePtr;
		}
		mmapKey->keySize = cur->keySize;
		mmapKey->keyUSize = cur->keyUSize;
		if (destType == TYPE_MMAP && keyValuePtr)
		{
			mmapKey->data.v = (void *) ((char *) keyValuePtr - mmapAddrInt);
		}
		else
		{
			mmapKey->data.v = keyValuePtr;
		}
		mmapKey->dataSize = cur->dataSize;
		if (destType == TYPE_MMAP && newMeta)
		{
			mmapKey->meta = (KeySet *) ((char *) newMeta - mmapAddrInt);
		}
		else
		{
			mmapKey->meta = newMeta;
		}
		mmapKey->ksReference = 1;

		// write the Key pointer into the KeySet array
		if (destType == TYPE_MMAP)
		{
			((Key **) ksArrayPtr)[keyIndex] = (Key *) ((char *) mmapKey - mmapAddrInt);
		}
		else
		{
			((Key **) ksArrayPtr)[keyIndex] = mmapKey;
		}

		++keyIndex;
	}

	ksPtr->flags = KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
	ksPtr->array = (Key **) ksArrayPtrOrig;
	ksPtr->array[keySet->size] = 0;
	if (destType == TYPE_MMAP) ksPtr->array = (Key **) ((char *) ksPtr->array - mmapAddrInt);
	ksPtr->alloc = keySet->alloc;
	ksPtr->size = keySet->size;
	ksPtr->cursor = 0;
	ksPtr->current = 0;
}

static void ksWrite (DestType destType, char * dest, KeySet * keySet, MmapHeader * mmapHeader, MmapFooter * mmapFooter, DynArray * dynArray)
{
	// multiple options for writing the KeySet:
	//		* fwrite () directly from the structs (needs multiple fwrite () calls)
	//		* memcpy () to continuous region and then fwrite () only once
	//		* use mmap to write to temp file and msync () after all data is written, then rename file
	mmapHeader->destAddr = dest;
	size_t mmapAddrInt = (size_t) mmapHeader->destAddr;

	KeySet * ksPtr;
	if (destType == TYPE_MMAP)
	{
		ksPtr = (KeySet *) (dest + SIZEOF_MMAPHEADER);
	}
	else
	{
		ksPtr = (KeySet *) dest;
	}

	if (keySet->size < 1)
	{
		char * ksArrayPtr = (((char *) ksPtr) + SIZEOF_KEYSET);
		ksPtr->flags = KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
		ksPtr->array = (Key **) ksArrayPtr;
		ksPtr->array[0] = 0;
		if (destType == TYPE_MMAP) ksPtr->array = (Key **) (ksArrayPtr - mmapAddrInt);
		ksPtr->alloc = keySet->alloc;
		ksPtr->size = 0;
		ksPtr->cursor = 0;
		ksPtr->current = 0;
	}
	else
	{
		writeKeySet (destType, mmapHeader, keySet, ksPtr, dynArray);
	}

	/* disable CRC
	char * ksCharPtr = (char *) ksPtr;
	uint32_t checksum = crc32 (0L, Z_NULL, 0);
	checksum = crc32 (checksum, ksCharPtr, (mmapHeader->mmapSize - SIZEOF_MMAPHEADER));

	mmapHeader->checksum = checksum;
	*/
	if (destType == TYPE_MMAP)
	{
		memcpy (dest, mmapHeader, SIZEOF_MMAPHEADER);
		memcpy ((dest + mmapHeader->allocSize - SIZEOF_MMAPFOOTER), mmapFooter, SIZEOF_MMAPFOOTER);
	}
}

static void mmapToKeySet (DestType destType, char * mappedRegion, KeySet * returned)
{
	KeySet * keySet;
	if (destType == TYPE_MMAP)
	{
		keySet = (KeySet *) (mappedRegion + SIZEOF_MMAPHEADER);
	}
	else
	{
		keySet = (KeySet *) mappedRegion;
	}
	returned->array = keySet->array;
	returned->size = keySet->size;
	returned->alloc = keySet->alloc;
	returned->cursor = 0;
	returned->current = 0;
	returned->mmapInfo = (MmapHeader *) mappedRegion;
	// to be able to free() the returned KeySet, just set the array flag here
	returned->flags = KS_FLAG_MMAP_ARRAY;
}

static void updatePointers (DestType destType, MmapHeader * mmapHeader, char * dest)
{
	size_t sourceInt = (size_t) mmapHeader->destAddr;
	size_t destInt = (size_t) dest;

	char * ksPtr;
	if (destType == TYPE_MMAP)
	{
		ksPtr = (dest + SIZEOF_MMAPHEADER);
	}
	else
	{
		ksPtr = dest - sourceInt + destInt;
	}

	char * ksArrayPtr = ksPtr + SIZEOF_KEYSET * mmapHeader->numKeySets;
	char * keyPtr = ksArrayPtr + SIZEOF_KEY_PTR * mmapHeader->ksAlloc;

	KeySet * ks;
	for (size_t i = 0; i < mmapHeader->numKeySets; ++i)
	{
		ks = (KeySet *) ksPtr;
		ksPtr += SIZEOF_KEYSET;
		if (ks->array)
		{
			if (destType == TYPE_MMAP)
			{
				ks->array = (Key **) ((char *) ks->array + destInt);
			}
			else
			{
				ks->array = (Key **) ((char *) ks->array - sourceInt + destInt);
			}

			for (size_t j = 0; j < ks->size; ++j)
			{
				if (ks->array[j])
				{
					if (destType == TYPE_MMAP)
					{
						ks->array[j] = (Key *) ((char *) (ks->array[j]) + destInt);
					}
					else
					{
						ks->array[j] = (Key *) ((char *) (ks->array[j]) - sourceInt + destInt);
					}
				}
			}
		}
	}

	Key * key;
	for (size_t i = 0; i < mmapHeader->numKeys; ++i)
	{
		key = (Key *) keyPtr;
		keyPtr += SIZEOF_KEY;
		if (key->data.v)
		{
			if (destType == TYPE_MMAP)
			{
				key->data.v = (void *) ((char *) (key->data.v) + destInt);
			}
			else
			{
				key->data.v = (void *) ((char *) (key->data.v) - sourceInt + destInt);
			}
		}
		if (key->key)
		{
			if (destType == TYPE_MMAP)
			{
				key->key = ((char *) (key->key) + destInt);
			}
			else
			{
				key->key = ((char *) (key->key) - sourceInt + destInt);
			}
		}
		if (key->meta)
		{
			if (destType == TYPE_MMAP)
			{
				key->meta = (KeySet *) ((char *) (key->meta) + destInt);
			}
			else
			{
				key->meta = (KeySet *) ((char *) (key->meta) - sourceInt + destInt);
			}
		}
	}
}

static void * hexStringToAddress (const char * hexString)
{
	int hexBase = 16;
	int errnosave = errno;
	char * endptr;
	errno = 0;

	long int val = strtol (hexString, &endptr, hexBase);

	/* Check for various possible errors */
	if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN)) || (errno != 0 && val == 0) || endptr == hexString)
	{
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		errno = errnosave;
		return 0;
	}

	errno = errnosave;
	return (void *) val;
}


KeySet * copyKeySet (MmapHeader * mmapInfo, KeySet * ks, MmapHeader * mmapHeader)
{
	if (!mmapInfo)
	{
		return 0;
	}

	if (test_bit (mmapInfo->flags, MMAP_FLAG_DELETED) == MMAP_FLAG_DELETED)
	{
		return 0;
	}

	// TODO: ultimately, use this function everywhere
	DynArray * dynArray = newDynArray ();

	// ksDeepDup (toCopy);

	calculateDataSize (TYPE_ALLOC, mmapHeader, ks, dynArray);
	mmapHeader->mmapMagicNumber = ELEKTRA_MAGIC_MMAP_NUMBER;

	size_t copySize = mmapHeader->allocSize;
	// char * dest = elektraCalloc (copySize);

	// TODO: if have MAP_ANONYMOUS
	char * dest = mmap ((void *) 0, copySize, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
	if (dest == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("error mapping MAP_ANONYMOUS");
		ELEKTRA_LOG_WARNING ("mmapSize: %zu", copySize);
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		return 0;
	}
	mmapHeader->destAddr = (char *) ks; // set old address
	ksWrite (TYPE_ALLOC, dest, ks, mmapHeader, 0, dynArray);

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

		void * toUnlinkKS = hexStringToAddress (keyString (cur));
		ELEKTRA_LOG_DEBUG ("unlink: unlinking KeySet str: %s", keyString (cur));
		ELEKTRA_LOG_DEBUG ("unlink: unlinking KeySet ptr: %p", toUnlinkKS);

		MmapHeader mmapHeader;
		memset (&mmapHeader, 0, SIZEOF_MMAPHEADER);
		KeySet * copy = copyKeySet (toUnlinkMmap, toUnlinkKS, &mmapHeader);
		if (copy)
		{
			ksClose (toUnlinkKS);
			updatePointers (TYPE_ALLOC, &mmapHeader, (char *) copy);
			mmapToKeySet (TYPE_ALLOC, (char *) copy, (KeySet *) toUnlinkKS);
			// elektraFree (copy); // not if MAP_ANONYMOUS
		}
		// keySetMeta (found, keyName (cur), ""); // delete
	}
}

/* -- Exported Elektra Plugin Functions ------------------------------------------------------------------------------------------------- */

int elektraMmapstorageOpen (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	KeySet * mappedFiles = ksNew (0, KS_END);
	elektraPluginSetData (handle, mappedFiles);

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
	if (readHeader (fp, &mmapHeader) == -1)
	{
		// config file was corrupt
		// TODO: check which error to set here
		ELEKTRA_LOG_WARNING ("could not read mmap information header");
		goto error;
	}

	char * mappedRegion = MAP_FAILED;
	ELEKTRA_LOG_DEBUG ("MMAP old addr: %p", (void *) mmapHeader.destAddr);
	mappedRegion = mmapFile ((void *) 0, fp, sbuf.st_size, MAP_PRIVATE, parentKey);
	ELEKTRA_LOG_DEBUG ("mappedRegion ptr: %p", (void *) mappedRegion);

	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("mappedRegion == MAP_FAILED");
		goto error;
	}
	ELEKTRA_LOG_DEBUG ("mappedRegion size: %zu", sbuf.st_size);
	ELEKTRA_LOG_DEBUG ("mappedRegion ptr: %p", (void *) mappedRegion);

	if (readFooter (mappedRegion, &mmapHeader) == -1)
	{
		// config file was corrupt/truncated
		// TODO: check which error to set here
		ELEKTRA_LOG_WARNING ("could not read mmap information footer: file was altered");
		goto error;
	}

	/* disable CRC
	char * ksPtr = (char *) (mappedRegion + SIZEOF_MMAPHEADER);
	uint32_t checksum = crc32 (0L, Z_NULL, 0);
	checksum = crc32 (checksum, ksPtr, mmapHeader.mmapSize - SIZEOF_MMAPHEADER);

	ELEKTRA_LOG_WARNING ("old checksum: %ul", mmapHeader.checksum);
	ELEKTRA_LOG_WARNING ("new checksum: %ul", checksum);
	if (checksum != mmapHeader.checksum)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG ("checksum failed");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	*/

	ksClose (returned);
	updatePointers (TYPE_MMAP, &mmapHeader, mappedRegion);
	mmapToKeySet (TYPE_MMAP, mappedRegion, returned);
	/*
	char * niceCopy = elektraMalloc (mmapHeader.mmapSize);
	memcpy (niceCopy, mappedRegion, mmapHeader.mmapSize);
	updatePointers (&mmapHeader, niceCopy);
	mmapToKeySet (niceCopy, returned);
	munmap (mappedRegion, mmapHeader.mmapSize);
	*/

	if (fclose (fp) != 0)
	{
		ELEKTRA_LOG_WARNING ("could not fclose");
		goto error;
	}

	// save keyset information to list of currently mmaped files
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

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;

error:
	ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
	ELEKTRA_SET_ERROR_GET (parentKey);

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
	Key * parentCopy = keyDup (parentKey);
	Key * found = ksLookup (mappedFiles, parentCopy, KDB_O_POP);
	if (found)
	{
		unlinkFile (found);
		keyDel (found);
	}
	keyDel (parentCopy);

	FILE * fp;

	if ((fp = openFile (parentKey, "w+")) == 0)
	{
		goto error;
	}

	dynArray = newDynArray ();

	MmapHeader mmapHeader;
	initHeader (&mmapHeader);
	calculateDataSize (TYPE_MMAP, &mmapHeader, returned, dynArray);
	ELEKTRA_LOG_DEBUG ("elektraMmapstorageSet -------> mmapsize: %zu", mmapHeader.allocSize);

	if (truncateFile (fp, mmapHeader.allocSize, parentKey) != 1)
	{
		goto error;
	}

	char * mappedRegion = mmapFile ((void *) 0, fp, mmapHeader.allocSize, MAP_SHARED, parentKey);
	ELEKTRA_LOG_DEBUG ("mappedRegion ptr: %p", (void *) mappedRegion);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("mappedRegion == MAP_FAILED");
		goto error;
	}

	MmapFooter mmapFooter;
	initFooter (&mmapFooter);
	ksWrite (TYPE_MMAP, mappedRegion, returned, &mmapHeader, &mmapFooter, dynArray);
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
	ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
	ELEKTRA_SET_ERROR_SET (parentKey);

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
