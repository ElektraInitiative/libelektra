/**
 * @file
 *
 * @brief Source for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */


#include "mmapstorage.h"

#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbprivate.h>

//#include <fcntl.h>	// open()
#include <assert.h>
#include <errno.h>
#include <stdio.h> // fopen(), fileno()
//#include <stdlib.h>    // exit()
#include <limits.h>    // SSIZE_MAX
#include <sys/mman.h>  // mmap()
#include <sys/stat.h>  // stat()
#include <sys/types.h> // ftruncate ()
#include <unistd.h>    // close(), ftruncate()

//#include <zlib.h> // crc32()


#define SIZEOF_KEY (sizeof (Key))
#define SIZEOF_KEY_PTR (sizeof (Key *))
#define SIZEOF_KEYSET (sizeof (KeySet))
#define SIZEOF_KEYSET_PTR (sizeof (KeySet *))
#define SIZEOF_MMAPHEADER (sizeof (MmapHeader))

static void m_output_meta (Key * k)
{
	const Key * meta;

	if (!k->meta)
	{
		ELEKTRA_LOG_WARNING ("Meta is NULL");
		return;
	}
	if (k->meta->size == 0)
	{
		ELEKTRA_LOG_WARNING ("Meta is empty");
	}

	ELEKTRA_LOG_WARNING ("Meta size: %zu", k->meta->size);

	keyRewindMeta (k);
	while ((meta = keyNextMeta (k)) != 0)
	{
		// ELEKTRA_LOG_WARNING ("Meta KeySet size: %zu", k->meta->size);
		if (!meta)
		{
			ELEKTRA_LOG_WARNING ("Meta Key is NULL");
		}
		ELEKTRA_LOG_WARNING (", %s: %s", keyName (meta), (const char *) keyValue (meta));
	}
	ELEKTRA_LOG_WARNING ("\n");
}

static void m_output_key (Key * k)
{
	// output_meta will print endline
	if (!k)
	{
		ELEKTRA_LOG_WARNING ("Key is NULL");
	}

	ELEKTRA_LOG_WARNING ("Key ptr: %p", (void *) k);
	ELEKTRA_LOG_WARNING ("keyname ptr: %p", (void *) k->key);
	ELEKTRA_LOG_WARNING ("keyname: %s", keyName (k));
	ELEKTRA_LOG_WARNING ("keystring ptr: %p", (void *) k->data.v);
	ELEKTRA_LOG_WARNING ("keystring: %s", keyString (k));
	m_output_meta (k);
}

static void m_output_keyset (KeySet * ks)
{
	ELEKTRA_LOG_WARNING ("-------------------> output keyset start");
	if (!ks)
	{

		ELEKTRA_LOG_WARNING ("KeySet is NULL");
		return;
	}
	if (ks->size == 0)
	{
		ELEKTRA_LOG_WARNING ("KeySet is empty");
	}

	ELEKTRA_LOG_WARNING ("KeySet size: %zu", ks->size);

	Key * k;
	ksRewind (ks);
	size_t ks_iterations = 0;
	while ((k = ksNext (ks)) != 0)
	{
		ELEKTRA_LOG_WARNING ("Key:");
		m_output_key (k);
		++ks_iterations;
	}
	ELEKTRA_LOG_WARNING ("KeySet iteration: %zu", ks_iterations);
	ELEKTRA_LOG_WARNING ("KeySet current: %zu", ks->current);
	ELEKTRA_LOG_WARNING ("KeySet size: %zu", ks->size);
	ELEKTRA_LOG_WARNING ("-------------------> output keyset done");
}

static FILE * mmapOpenFile (Key * parentKey, const char * mode)
{
	FILE * fp;
	ELEKTRA_LOG ("opening file %s", keyString (parentKey));

	if ((fp = fopen (keyString (parentKey), mode)) == 0)
	{
		ELEKTRA_LOG_WARNING ("error opening file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
	}
	return fp;
}

static int mmapTruncateFile (FILE * fp, size_t mmapsize, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("truncating file %s", keyString (parentKey));

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

static int mmapStat (struct stat * sbuf, Key * parentKey)
{
	ELEKTRA_LOG ("stat() on file %s", keyString (parentKey));

	if (stat (keyString (parentKey), sbuf) == -1)
	{
		ELEKTRA_LOG_WARNING ("error on stat() for file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		return -1;
	}
	return 1;
}

static char * mmapMapFile (void * addr, FILE * fp, size_t mmapSize, int mapOpts, Key * parentKey ELEKTRA_UNUSED)
{
	ELEKTRA_LOG ("mapping file %s", keyString (parentKey));

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

#ifdef DEBUG
int findOrInsert (Key * key, DynArray * dynArray)
#else
static int findOrInsert (Key * key, DynArray * dynArray)
#endif
{
	size_t l = 0;
	size_t h = dynArray->size;
	size_t m;
	ELEKTRA_LOG_WARNING ("l: %zu", l);
	ELEKTRA_LOG_WARNING ("h: %zu", h);
	ELEKTRA_LOG_WARNING ("dynArray->size: %zu", dynArray->size);
	ELEKTRA_LOG_WARNING ("dynArray->alloc: %zu", dynArray->alloc);

	while (l < h)
	{
		m = (l + h) >> 1;
		ELEKTRA_LOG_WARNING ("m: %zu", m);

		if (dynArray->keyArray[m] > key)
			h = m;
		else if (dynArray->keyArray[m] < key)
			l = ++m;
		else
			return 1; // found
	}
	// insert key at index l
	if (dynArray->size == dynArray->alloc)
	{
		// doubling the array size to keep reallocations logarithmic
		size_t oldAllocSize = dynArray->alloc;
		Key ** new = elektraCalloc ((2 * oldAllocSize) * sizeof (Key *));
		memcpy (new, dynArray->keyArray, dynArray->size * sizeof (Key *));
		elektraFree (dynArray->keyArray);
		dynArray->keyArray = new;
		dynArray->alloc = 2 * oldAllocSize;
	}

	memmove ((void *) (dynArray->keyArray + l + 1), (void *) (dynArray->keyArray + l), ((dynArray->size) - l) * (sizeof (size_t)));
	dynArray->keyArray[l] = key;
	dynArray->size += 1;

	return 0;
}

static size_t find (Key * key, DynArray * dynArray)
{
	size_t l = 0;
	size_t h = dynArray->size;
	size_t m;
	ELEKTRA_LOG_WARNING ("l: %zu", l);
	ELEKTRA_LOG_WARNING ("h: %zu", h);
	ELEKTRA_LOG_WARNING ("dynArray->size: %zu", dynArray->size);
	ELEKTRA_LOG_WARNING ("dynArray->alloc: %zu", dynArray->alloc);

	while (l < h)
	{
		m = (l + h) >> 1;
		ELEKTRA_LOG_WARNING ("m: %zu", m);

		if (dynArray->keyArray[m] > key)
			h = m;
		else if (dynArray->keyArray[m] < key)
			l = ++m;
		else
			return m; // found
	}

	return -1;
}


static void mmapDataSize (MmapHeader * mmapHeader, KeySet * returned, DynArray * dynArray)
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
				if (findOrInsert (curMeta, dynArray) != 1)
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

	size_t mmapSize = SIZEOF_MMAPHEADER + SIZEOF_KEYSET + keyPtrArraySize + keyArraySize + dataBlocksSize +
			  (metaKeySets * SIZEOF_KEYSET) + (metaKsAlloc * SIZEOF_KEY_PTR) + (dynArray->size * SIZEOF_KEY);

	mmapHeader->mmapSize = mmapSize;
	mmapHeader->numKeySets = 1 + metaKeySets;
	mmapHeader->ksAlloc = returned->alloc + metaKsAlloc;
	mmapHeader->numKeys = returned->size + dynArray->size;
	mmapHeader->dataSize = dataBlocksSize;
}

static void writeKeySet (MmapHeader * mmapHeader, KeySet * keySet, KeySet * dest, DynArray * dynArray)
{
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
	ELEKTRA_LOG_WARNING ("writing META keys");
	Key * mmapMetaKey;
	Key * curMeta;
	void * metaKeyNamePtr;
	void * metaKeyValuePtr;
	for (size_t i = 0; i < dynArray->size; ++i)
	{
		ELEKTRA_LOG_WARNING ("index: %zu", i);
		curMeta = dynArray->keyArray[i]; // old key location
		mmapMetaKey = (Key *) keyPtr;    // new key location
		keyPtr += SIZEOF_KEY;

		ELEKTRA_LOG_WARNING ("meta mmap location ptr: %p", (void *) mmapMetaKey);
		ELEKTRA_LOG_WARNING ("meta old location ptr: %p", (void *) curMeta);
		ELEKTRA_LOG_WARNING ("%p key: %s, string: %s", (void *) curMeta, keyName (curMeta), keyString (curMeta));

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
		mmapMetaKey->key = metaKeyNamePtr;
		mmapMetaKey->data.v = metaKeyValuePtr;
		mmapMetaKey->meta = 0;
		mmapMetaKey->ksReference = 0;
		mmapMetaKey->dataSize = curMeta->dataSize;
		mmapMetaKey->keySize = curMeta->keySize;
		mmapMetaKey->keyUSize = curMeta->keyUSize;

		dynArray->mappedKeyArray[i] = mmapMetaKey;
		ELEKTRA_LOG_WARNING ("NEW MMAP META KEY:");
		m_output_key (mmapMetaKey);
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
			ELEKTRA_LOG_WARNING ("this key has meta info");
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
				ELEKTRA_LOG_WARNING ("mappedMetaKey: %p", (void *) mappedMetaKey);
				newMeta->array[metaKeyIndex] = mappedMetaKey;
				if (mappedMetaKey->ksReference < SSIZE_MAX) ++(mappedMetaKey->ksReference);
				++metaKeyIndex;
			}
			newMeta->array[oldMeta->size] = 0;
			newMeta->alloc = oldMeta->alloc;
			newMeta->size = oldMeta->size;
			newMeta->cursor = 0;
			newMeta->current = 0;
		}
		ELEKTRA_LOG_WARNING ("INSERT META INTO REAL KEY, HERE IS THE META KS:");
		m_output_keyset (newMeta);


		// move Key itself
		mmapKey->flags = KEY_FLAG_MMAP_STRUCT | KEY_FLAG_MMAP_KEY | KEY_FLAG_MMAP_DATA;
		mmapKey->key = keyNamePtr;
		mmapKey->keySize = cur->keySize;
		mmapKey->keyUSize = cur->keyUSize;
		mmapKey->data.v = keyValuePtr;
		mmapKey->dataSize = cur->dataSize;
		mmapKey->meta = newMeta;
		mmapKey->ksReference = 1;

		ELEKTRA_LOG_WARNING ("wrote new Key and meta KS is at: %p", (void *) newMeta);

		// write the Key pointer into the KeySet array
		((Key **) ksArrayPtr)[keyIndex] = mmapKey;

		++keyIndex;
	}

	ksPtr->flags = KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
	ksPtr->array = (Key **) ksArrayPtrOrig;
	ksPtr->array[keySet->size] = 0;
	ksPtr->alloc = keySet->alloc;
	ksPtr->size = keySet->size;
	ksPtr->cursor = 0;
	ksPtr->current = 0;
}

static void mmapWrite (char * mappedRegion, KeySet * keySet, MmapHeader * mmapHeader, DynArray * dynArray)
{
	// multiple options for writing the KeySet:
	//		* fwrite () directly from the structs (needs multiple fwrite () calls)
	//		* memcpy () to continuous region and then fwrite () only once
	//		* use mmap to write to temp file and msync () after all data is written, then rename file
	mmapHeader->mmapAddr = mappedRegion;

	KeySet * ksPtr = (KeySet *) (mappedRegion + SIZEOF_MMAPHEADER);

	if (keySet->size < 1)
	{
		char * ksArrayPtr = (((char *) ksPtr) + SIZEOF_KEYSET);
		ksPtr->flags = KS_FLAG_MMAP_STRUCT | KS_FLAG_MMAP_ARRAY;
		ksPtr->array = (Key **) ksArrayPtr;
		ksPtr->array[0] = 0;
		ksPtr->alloc = keySet->alloc;
		ksPtr->size = 0;
		ksPtr->cursor = 0;
		ksPtr->current = 0;
	}
	else
	{
		writeKeySet (mmapHeader, keySet, ksPtr, dynArray);
	}

	/* disable CRC
	char * ksCharPtr = (char *) ksPtr;
	uint32_t checksum = crc32 (0L, Z_NULL, 0);
	checksum = crc32 (checksum, ksCharPtr, (mmapHeader->mmapSize - SIZEOF_MMAPHEADER));

	mmapHeader->checksum = checksum;
	*/
	memcpy (mappedRegion, mmapHeader, SIZEOF_MMAPHEADER);
}

static void mmapToKeySet (char * mappedRegion, KeySet * returned)
{
	KeySet * keySet = (KeySet *) (mappedRegion + SIZEOF_MMAPHEADER);
	returned->array = keySet->array;
	returned->size = keySet->size;
	returned->alloc = keySet->alloc;
	returned->cursor = 0;
	returned->current = 0;
	// to be able to free() the returned KeySet, just set the array flag here
	returned->flags = KS_FLAG_MMAP_ARRAY;
}

static int readMmapHeader (FILE * fp, MmapHeader * mmapHeader)
{
	memset (mmapHeader, 0, SIZEOF_MMAPHEADER * (sizeof (char)));
	fread (mmapHeader, SIZEOF_MMAPHEADER, (sizeof (char)), fp);

	if (mmapHeader->mmapMagicNumber == ELEKTRA_MAGIC_MMAP_NUMBER) return 0;

	return -1;
}

static void updatePointers (MmapHeader * mmapHeader, char * dest)
{
	size_t sourceInt = (size_t) mmapHeader->mmapAddr;
	size_t destInt = (size_t) dest;

	char * ksPtr = dest + SIZEOF_MMAPHEADER;
	char * ksArrayPtr = ksPtr + SIZEOF_KEYSET * mmapHeader->numKeySets;
	char * keyPtr = ksArrayPtr + SIZEOF_KEY_PTR * mmapHeader->ksAlloc;

	KeySet * ks;
	for (size_t i = 0; i < mmapHeader->numKeySets; ++i)
	{
		ks = (KeySet *) ksPtr;
		ksPtr += SIZEOF_KEYSET;
		if (ks->array)
		{
			ks->array = (Key **) ((char *) ks->array - sourceInt + destInt);
			for (size_t j = 0; j < ks->size; ++j)
			{
				if (ks->array[j]) ks->array[j] = (Key *) ((char *) (ks->array[j]) - sourceInt + destInt);
			}
		}
	}

	Key * key;
	for (size_t i = 0; i < mmapHeader->numKeys; ++i)
	{
		key = (Key *) keyPtr;
		keyPtr += SIZEOF_KEY;
		if (key->data.v) key->data.v = (void *) ((char *) (key->data.v) - sourceInt + destInt);
		if (key->key) key->key = ((char *) (key->key) - sourceInt + destInt);
		if (key->meta) key->meta = (KeySet *) ((char *) (key->meta) - sourceInt + destInt);
	}
}

int elektraMmapstorageOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
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
	// get all keys

	int errnosave = errno;
	FILE * fp;

	if ((fp = mmapOpenFile (parentKey, "r+")) == 0)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	struct stat sbuf;
	if (mmapStat (&sbuf, parentKey) != 1)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		fclose (fp);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (sbuf.st_size == 0)
	{
		// empty mmap file
		fclose (fp);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	MmapHeader mmapHeader;
	if (readMmapHeader (fp, &mmapHeader) == -1)
	{
		// config file was corrupt
		// TODO: check which error to set here
		ELEKTRA_LOG ("could not read mmap information header");
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		fclose (fp);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	char * mappedRegion = MAP_FAILED;
	ELEKTRA_LOG_WARNING ("MMAP old addr: %p", (void *) mmapHeader.mmapAddr);
	mappedRegion = mmapMapFile ((void *) 0, fp, sbuf.st_size, MAP_PRIVATE, parentKey);
	ELEKTRA_LOG_WARNING ("mappedRegion ptr: %p", (void *) mappedRegion);

	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		fclose (fp);
		ELEKTRA_LOG ("mappedRegion == MAP_FAILED");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	ELEKTRA_LOG_WARNING ("mappedRegion size: %zu", sbuf.st_size);
	ELEKTRA_LOG_WARNING ("mappedRegion ptr: %p", (void *) mappedRegion);

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
	updatePointers (&mmapHeader, mappedRegion);
	mmapToKeySet (mappedRegion, returned);
	/*
	char * niceCopy = elektraMalloc (mmapHeader.mmapSize);
	memcpy (niceCopy, mappedRegion, mmapHeader.mmapSize);
	updatePointers (&mmapHeader, niceCopy);
	mmapToKeySet (niceCopy, returned);
	munmap (mappedRegion, mmapHeader.mmapSize);
	*/
	fclose (fp);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// set all keys
	m_output_keyset (returned);

	int errnosave = errno;
	FILE * fp;

	if ((fp = mmapOpenFile (parentKey, "w+")) == 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	DynArray dynArray;
	dynArray.keyArray = elektraCalloc (sizeof (Key *));
	dynArray.mappedKeyArray = 0;
	dynArray.size = 0;
	dynArray.alloc = 1;

	MmapHeader mmapHeader;
	mmapDataSize (&mmapHeader, returned, &dynArray);
	mmapHeader.mmapMagicNumber = ELEKTRA_MAGIC_MMAP_NUMBER;
	ELEKTRA_LOG_WARNING ("elektraMmapstorageSet -------> mmapsize: %zu", mmapHeader.mmapSize);

	if (mmapTruncateFile (fp, mmapHeader.mmapSize, parentKey) != 1)
	{
		fclose (fp);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	char * mappedRegion = mmapMapFile ((void *) 0, fp, mmapHeader.mmapSize, MAP_SHARED, parentKey);
	ELEKTRA_LOG_WARNING ("mappedRegion ptr: %p", (void *) mappedRegion);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		fclose (fp);
		ELEKTRA_LOG ("mappedRegion == MAP_FAILED");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	mmapWrite (mappedRegion, returned, &mmapHeader, &dynArray);
	if (msync ((void *) mappedRegion, mmapHeader.mmapSize, MS_SYNC) != 0)
	{
		ELEKTRA_LOG ("could not msync");
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		fclose (fp);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	munmap (mappedRegion, mmapHeader.mmapSize);

	if (dynArray.keyArray) elektraFree (dynArray.keyArray);
	if (dynArray.mappedKeyArray) elektraFree (dynArray.mappedKeyArray);
	fclose (fp);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
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
