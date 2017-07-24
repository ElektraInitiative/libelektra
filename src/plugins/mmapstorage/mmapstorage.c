/**
 * @file
 *
 * @brief Source for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */



#include "mmapstorage.h"

#include <kdbhelper.h>
#include <kdberrors.h>
#include <kdbprivate.h>

//#include <fcntl.h>	// open()
#include <errno.h>
#include <stdio.h>	// fopen()
#include <unistd.h>	// close(), ftruncate()
#include <sys/mman.h>	// mmap()
#include <sys/stat.h>	// stat()
#include <sys/types.h>	// ftruncate ()
#include <kdblogger.h>

#define SIZEOF_KEY		(sizeof (Key))
#define SIZEOF_KEY_PTR		(sizeof (Key *))
#define SIZEOF_KEYSET		(sizeof (KeySet))
#define SIZEOF_KEYSET_PTR	(sizeof (KeySet *))
#define SIZEOF_MMAPHEADER	(sizeof (MmapHeader))


static FILE * elektraMmapstorageOpenFile (Key * parentKey, int errnosave)
{
	FILE * fp;
	ELEKTRA_LOG ("opening file %s", keyString (parentKey));

	if ((fp = fopen (keyString (parentKey), "rw+")) == 0) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error opening file %s", keyString (parentKey));
	}
	return fp;
}

static int elektraMmapstorageTruncateFile (FILE * fp, size_t mmapsize, Key * parentKey, int errnosave)
{
	ELEKTRA_LOG ("truncating file %s", keyString (parentKey));

	int fd = fileno (fp);
	if ((ftruncate (fd, mmapsize)) == -1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		ELEKTRA_LOG_WARNING ("error truncating file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("mmapsize: %zu", mmapsize);
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		errno = errnosave;
		return -1;
	}
	return 1;
}

static int elektraMmapstorageStat (struct stat * sbuf, Key * parentKey, int errnosave)
{
	ELEKTRA_LOG ("stat() on file %s", keyString (parentKey));

	if (stat(keyString (parentKey), sbuf) == -1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error on stat() for file %s", keyString (parentKey));
		return -1;
	}
	return 1;
}

static char * elektraMmapstorageMapFile (void * addr, FILE * fp, size_t mmapSize, int mapOpts , Key * parentKey, int errnosave)
{
	ELEKTRA_LOG ("mapping file %s", keyString (parentKey));

	int fd = fileno (fp);
	char * mappedRegion = mmap (addr, mmapSize, PROT_READ | PROT_WRITE, mapOpts, fd, 0);
	if (mappedRegion == MAP_FAILED) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error mapping file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("mmapSize: %zu", mmapSize);
		return MAP_FAILED;
	}
	return mappedRegion;
}

static MmapHeader elektraMmapstorageDataSize (KeySet * returned)
{
	MmapHeader ret;
	
	Key * cur;
	ksRewind(returned);
	size_t dynamicDataSize = 0;
	size_t metaKeys = 0;
	while ((cur = ksNext (returned)) != 0)
	{
		dynamicDataSize += (keyGetNameSize(cur) + keyGetValueSize(cur));
		
		if (cur->meta)
			metaKeys += ksGetSize(cur->meta);
	}

	size_t keyArraySize = (returned->size) * SIZEOF_KEY;
	size_t keyPtrArraySize = (returned->size) * SIZEOF_KEY_PTR;
	
	// TODO: overapproximating meta key size here, fix later
	size_t mmapSize = SIZEOF_MMAPHEADER + SIZEOF_KEYSET + keyArraySize + dynamicDataSize \
		+ keyPtrArraySize + (metaKeys * SIZEOF_KEY) + ((returned->size) * SIZEOF_KEYSET);
		
	size_t ksSize = SIZEOF_MMAPHEADER + SIZEOF_KEYSET + keyArraySize + dynamicDataSize + keyPtrArraySize;

	ret.mmapSize = mmapSize;
	ret.numKeys = ksSize;
	ret.numMetaKeys = metaKeys;
	return ret;
}

static int findOrInsert (const size_t key, size_t * array, size_t numElements)
{
	size_t l = 0;
	size_t h = numElements-1;
	size_t m;
	
	while (l <= h)
	{
		m = (l+h)>>1;
		
		if (array[m] > key)
			h = --m;
		else if (array[m] < key)
			l = ++m;
		else
			return 1; // found
	}
	// insert key at index l, relies on the fact that enough memory has already been allocated for array
	memmove ((void *) (array+l+1), (void *) (array+l), (numElements-l) * (sizeof (size_t)));
	array[l] = key;
	return 0;
}

static void elektraMmapstorageWriteKeySet (char * mappedRegion, KeySet * keySet, MmapHeader mmapHeader)
{
	// multiple options for writing the KeySet:
	//		* fwrite () directly from the structs (needs multiple fwrite () calls)
	//		* memcpy () to continuous region and then fwrite () only once
	//		* use mmap to write to temp file and msync () after all data is written, then rename file
	//MmapHeader mmapHeader;
	mmapHeader.addr = mappedRegion;
	memcpy (mappedRegion, &mmapHeader, SIZEOF_MMAPHEADER);
	
	
	DynArray dynArray;
	dynArray.array = calloc (mmapHeader.numMetaKeys, sizeof (size_t));
	dynArray.size = 0;
	
// 	void * metaKeys[mmapSize.metaKeys];
// 	memset (metaKeys, 0, (sizeof (void *) * mmapSize.metaKeys));
// // 	size_t metaKeysNum = 0;

	char * ksRegion = mappedRegion + SIZEOF_MMAPHEADER;

	if (keySet->size < 1)
	{
		// TODO: reduce this code duplication
		// move KeySet itself to the mapped region
		keySet->flags |= KS_FLAG_MMAP;
		memcpy (ksRegion, keySet, SIZEOF_KEYSET);
		return;
	}

	size_t keyArraySize = (keySet->size) * SIZEOF_KEY;
	size_t keyPtrArraySize = (keySet->size) * SIZEOF_KEY_PTR;

	size_t dataOffset = SIZEOF_KEYSET + keyArraySize; // ptr to start of DATA block
	char * dataNextFreeBlock = ksRegion + dataOffset;
	char * metaPtr = mappedRegion + (mmapHeader.numKeys * sizeof(Key));
	KeySet * metaCopies = ksNew (0, KS_END);

	Key * cur;
	Key ** mappedKeys = elektraMalloc (keyPtrArraySize);
	size_t keyIndex = 0;
	ksRewind(keySet);
	while ((cur = ksNext (keySet)) != 0)
	{
		size_t keyNameSize = keyGetNameSize(cur);
		size_t keyValueSize = keyGetValueSize(cur);

		// move Key name
		memcpy (dataNextFreeBlock, keyName(cur), keyNameSize);
		//fwrite (keyName(cur), keyNameSize, 1, fp);
		
		keySetName(cur, dataNextFreeBlock);
		dataNextFreeBlock += keyNameSize;

		// move Key value
		memcpy (dataNextFreeBlock, keyValue(cur), keyValueSize);
		//fwrite (keyValue(cur), keyValueSize, 1, fp);
		keySetRaw(cur, dataNextFreeBlock, keyValueSize);
		dataNextFreeBlock += keyValueSize;
		
		// meta key search and so on
		const Key * meta;
		keyRewindMeta (cur);
		while ((meta = keyNextMeta (cur)) != 0)
		{
			int found = findOrInsert ((const size_t) meta, dynArray.array, dynArray.size);
			
			if (!found)
			{
				// write to mapped region (metaPtr)
				
			}
			else
			{
				// already in mapped region, reference it
				
			}
		}

		// move Key itself
		void * mmapKey = ksRegion + SIZEOF_KEYSET + (keyIndex * SIZEOF_KEY);
		cur->flags |= KEY_FLAG_MMAP;
		memcpy (mmapKey, cur, SIZEOF_KEY);
		//fwrite (cur, keyValueSize, 1, fp);
		
		// remember pointer to Key for our root KeySet
		mappedKeys[keyIndex] = mmapKey;
		
		++keyIndex;
	}
	// copy key ptrs from array to DATA block
	memcpy (dataNextFreeBlock, mappedKeys, keyPtrArraySize);
	keySet->array = (Key **) dataNextFreeBlock;
	dataNextFreeBlock += keyPtrArraySize;
	elektraFree (mappedKeys);

	// move KeySet itself to the mapped region
	keySet->flags |= KS_FLAG_MMAP;
	memcpy (ksRegion, keySet, SIZEOF_KEYSET);
	
	ksDel (metaCopies);
}

static void mmapToKeySet (char * mappedRegion, KeySet * returned)
{
	KeySet * keySet = (KeySet *) (mappedRegion + SIZEOF_MMAPHEADER);
	returned->array = keySet->array;
	returned->size 	= keySet->size;
	returned->alloc = keySet->alloc;
	ksRewind(returned); // cursor = 0; current = 0
	returned->flags = keySet->flags;
}

static void * mmapAddr (FILE * fp)
{
	char buf[SIZEOF_MMAPHEADER];
	memset (buf, 0, SIZEOF_MMAPHEADER * (sizeof (char)));
	
	fread (buf, SIZEOF_MMAPHEADER, (sizeof (char)), fp);
	MmapHeader * mmapHeader = (MmapHeader *) buf;
	
	if (mmapHeader->mmapMagicNumber == ELEKTRA_MAGIC_MMAP_NUMBER)
		return mmapHeader->addr;
	
	return 0;
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

	// munmap (mappedRegion, sbuf.st_size);
	// close (fd);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/mmapstorage"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/mmapstorage", KEY_VALUE, "mmapstorage plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/mmapstorage/exports", KEY_END),
			       keyNew ("system/elektra/modules/mmapstorage/exports/open", KEY_FUNC, elektraMmapstorageOpen, KEY_END),
			       keyNew ("system/elektra/modules/mmapstorage/exports/close", KEY_FUNC, elektraMmapstorageClose, KEY_END),
			       keyNew ("system/elektra/modules/mmapstorage/exports/get", KEY_FUNC, elektraMmapstorageGet, KEY_END),
			       keyNew ("system/elektra/modules/mmapstorage/exports/set", KEY_FUNC, elektraMmapstorageSet, KEY_END),
			       keyNew ("system/elektra/modules/mmapstorage/exports/error", KEY_FUNC, elektraMmapstorageError, KEY_END),
			       keyNew ("system/elektra/modules/mmapstorage/exports/checkconf", KEY_FUNC, elektraMmapstorageCheckConfig, KEY_END),
#include ELEKTRA_README (mmapstorage)
			       keyNew ("system/elektra/modules/mmapstorage/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	int errnosave = errno;
	FILE * fp;

	if ((fp = elektraMmapstorageOpenFile (parentKey, errnosave)) == 0)
	{
		return -1;
	}

	struct stat sbuf;
	if (elektraMmapstorageStat (&sbuf, parentKey, errnosave) != 1)
	{
		fclose (fp);
		return -1;
	}
	
	void * addr = mmapAddr (fp);
	char * mappedRegion = MAP_FAILED;
	
	// can not use MAP_FIXED with addr = 0
	if (!addr)
		mappedRegion = elektraMmapstorageMapFile (addr, fp, sbuf.st_size, MAP_PRIVATE, parentKey, errnosave);
	else
		mappedRegion = elektraMmapstorageMapFile (addr, fp, sbuf.st_size, MAP_PRIVATE | MAP_FIXED, parentKey, errnosave);
	
	if (mappedRegion == MAP_FAILED)
	{
		fclose (fp);
		ELEKTRA_LOG ("mappedRegion == MAP_FAILED");
		return -1;
	}
	ELEKTRA_LOG_WARNING ("mappedRegion size: %zu", sbuf.st_size);
	ELEKTRA_LOG_WARNING ("mappedRegion ptr: %p", (void *) mappedRegion);

	mmapToKeySet (mappedRegion, returned);

	//munmap(mappedRegion, sbuf.st_size);
	fclose (fp);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// set all keys

	int errnosave = errno;
	FILE * fp;

	if ((fp = elektraMmapstorageOpenFile (parentKey, errnosave)) == 0)
	{
		return -1;
	}

	// TODO: calculating mmap size not needed if using fwrite() instead of mmap to write to file
	MmapHeader mmapHeader = elektraMmapstorageDataSize (returned);
	mmapHeader.mmapMagicNumber = ELEKTRA_MAGIC_MMAP_NUMBER;
	ELEKTRA_LOG_WARNING ("elektraMmapstorageSet -------> mmapsize: %zu", mmapHeader.mmapSize);

	if (elektraMmapstorageTruncateFile (fp, mmapHeader.mmapSize, parentKey, errnosave) != 1)
	{
		fclose (fp);
		return -1;
	}

	char * mappedRegion = elektraMmapstorageMapFile ((void *) 0, fp, mmapHeader.mmapSize, MAP_SHARED, parentKey, errnosave);
	ELEKTRA_LOG_WARNING ("mappedRegion ptr: %p", (void *) mappedRegion);
	if (mappedRegion == MAP_FAILED)
	{
		fclose (fp);
		ELEKTRA_LOG ("mappedRegion == MAP_FAILED");
		return -1;
	}

	elektraMmapstorageWriteKeySet (mappedRegion, returned, mmapHeader);

	//ksCopy(returned, (KeySet *) mappedRegion);
	mmapToKeySet (mappedRegion, returned);

	//munmap(mappedRegion, mmapsize);
	fclose (fp);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (mmapstorage)
{
	// clang-format off
	return elektraPluginExport ("mmapstorage",
		ELEKTRA_PLUGIN_OPEN,	&elektraMmapstorageOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraMmapstorageClose,
		ELEKTRA_PLUGIN_GET,	&elektraMmapstorageGet,
		ELEKTRA_PLUGIN_SET,	&elektraMmapstorageSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraMmapstorageError,
		ELEKTRA_PLUGIN_END);
}
