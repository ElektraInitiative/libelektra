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
#include <kdblogger.h>

//#include <fcntl.h>	// open()
#include <errno.h>
#include <stdio.h>	// fopen()
#include <unistd.h>	// close(), ftruncate()
#include <sys/mman.h>	// mmap()
#include <sys/stat.h>	// stat()
#include <sys/types.h>	// ftruncate ()
#include <assert.h>


#define SIZEOF_KEY		(sizeof (Key))
#define SIZEOF_KEY_PTR		(sizeof (Key *))
#define SIZEOF_KEYSET		(sizeof (KeySet))
#define SIZEOF_KEYSET_PTR	(sizeof (KeySet *))
#define SIZEOF_MMAPHEADER	(sizeof (MmapHeader))

static void m_output_meta (Key * k)
{
	const Key * meta;
	
	if (!k->meta)
	{
		ELEKTRA_LOG_WARNING ("Meta is NULL");
		return;
	}
	if(k->meta->size == 0)
		ELEKTRA_LOG_WARNING ("Meta is empty");
	
	ELEKTRA_LOG_WARNING ("Meta size: %zu", k->meta->size);

	keyRewindMeta (k);
	while ((meta = keyNextMeta (k)) != 0)
	{
		//ELEKTRA_LOG_WARNING ("Meta KeySet size: %zu", k->meta->size);
		if (!meta)
			ELEKTRA_LOG_WARNING ("Meta Key is NULL");
		ELEKTRA_LOG_WARNING (", %s: %s", keyName (meta), (const char *)keyValue (meta));
	}
	ELEKTRA_LOG_WARNING ("\n");
}

static void m_output_key (Key * k)
{
	// output_meta will print endline
	if (!k)
		ELEKTRA_LOG_WARNING ("Key is NULL");
		
	ELEKTRA_LOG_WARNING ("Key ptr: %p", (void *)k);
	ELEKTRA_LOG_WARNING ("keyname ptr: %p", (void *)k->key);
	ELEKTRA_LOG_WARNING ("keyname: %s", keyName (k));
	ELEKTRA_LOG_WARNING ("keystring ptr: %p", (void *)k->data.v);
	ELEKTRA_LOG_WARNING ("keystring: %s", keyString (k));
	m_output_meta (k);
}

static void m_output_keyset (KeySet * ks)
{
	ELEKTRA_LOG_WARNING ("-------------------> output keyset start");
	if (!ks) {
		
		ELEKTRA_LOG_WARNING ("KeySet is NULL");
		return;
	}
	if(ks->size == 0)
		ELEKTRA_LOG_WARNING ("KeySet is empty");
	
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

static FILE * elektraMmapstorageOpenFile (Key * parentKey, const char * mode, int errnosave)
{
	FILE * fp;
	ELEKTRA_LOG ("opening file %s", keyString (parentKey));

	if ((fp = fopen (keyString (parentKey), mode)) == 0) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		ELEKTRA_LOG_WARNING ("error opening file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		errno = errnosave;
	}
	return fp;
}

static int elektraMmapstorageTruncateFile (FILE * fp, size_t mmapsize, Key * parentKey, int errnosave)
{
	ELEKTRA_LOG ("truncating file %s", keyString (parentKey));

	// TODO: does it matter whether we use truncate or ftruncate?
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
		ELEKTRA_LOG_WARNING ("error on stat() for file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		errno = errnosave;
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
		ELEKTRA_LOG_WARNING ("error mapping file %s", keyString (parentKey));
		ELEKTRA_LOG_WARNING ("mmapSize: %zu", mmapSize);
		ELEKTRA_LOG_WARNING ("strerror: %s", strerror (errno));
		errno = errnosave;
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
		m = (l+h)>>1;
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
		Key ** new = calloc (2 * oldAllocSize, sizeof (Key *));
		memcpy (new, dynArray->keyArray, dynArray->size * sizeof (Key *));
		free (dynArray->keyArray);
		dynArray->keyArray = new;
		dynArray->alloc = 2 * oldAllocSize;
	}

	memmove ((void *) (dynArray->keyArray+l+1), (void *) (dynArray->keyArray+l), ((dynArray->size)-l) * (sizeof (size_t)));
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
		m = (l+h)>>1;
		ELEKTRA_LOG_WARNING ("m: %zu", m);
		
		if (dynArray->keyArray[m] > key)
			h = m;
		else if (dynArray->keyArray[m] < key)
			l = ++m;
		else
			return m; // found
	}
	
	assert (0);
}


static MmapHeader elektraMmapstorageDataSize (KeySet * returned, DynArray * dynArray)
{
	MmapHeader ret;
	
	Key * cur;
	ksRewind(returned);
	size_t dataBlocksSize = 0; // keyName and keyValue
	size_t metaKeySets = 0;
	size_t metaKeysAlloc = 0; // number of meta keys including copies
	while ((cur = ksNext (returned)) != 0)
	{
		dataBlocksSize += (cur->keySize + cur->keyUSize + cur->dataSize);
		
		if (cur->meta)
		{
			++metaKeySets;
			
			Key * curMeta;
			ksRewind(cur->meta);
			while ((curMeta = ksNext (cur->meta)) != 0)
			{
				if (findOrInsert (curMeta, dynArray) != 1)
				{
					// key was just inserted
					dataBlocksSize += (curMeta->keySize + curMeta->keyUSize + curMeta->dataSize);
				}
			}
			metaKeysAlloc += (cur->meta->alloc);
		}
			
	}

	size_t keyArraySize = (returned->size) * SIZEOF_KEY;
	size_t keyPtrArraySize = (returned->alloc) * SIZEOF_KEY_PTR;

	size_t mmapSize = SIZEOF_MMAPHEADER + SIZEOF_KEYSET + keyPtrArraySize + keyArraySize + dataBlocksSize \
 		+ (metaKeySets * SIZEOF_KEYSET) + (metaKeysAlloc * SIZEOF_KEY_PTR) + (dynArray->size * SIZEOF_KEY);

	ret.mmapSize = mmapSize;
	ret.dataSize = dataBlocksSize;
	ret.numKeys = returned->size;
	ret.numMetaKeySets = metaKeySets;
	ret.numMetaKeys = dynArray->size;
	return ret;
}

static void writeKeySet (MmapHeader * mmapHeader, KeySet * keySet, KeySet * dest, DynArray * dynArray)
{
	KeySet * ksPtr = dest;
	char * ksArrayPtr = (((char *) ksPtr) + SIZEOF_KEYSET);
	char * keyPtr = (ksArrayPtr + ((keySet->alloc) * SIZEOF_KEY_PTR));
	char * dataPtr = (keyPtr + (keySet->size * SIZEOF_KEY));
	char * metaPtr = (dataPtr + (mmapHeader->dataSize));
	char * metaKsPtr = (metaPtr + (dynArray->size * sizeof (Key)));
	ELEKTRA_LOG_WARNING ("metaKsPtr: %p", (void *) metaKsPtr);
	
	// allocate space in DynArray to remember the addresses of meta keys
	dynArray->mappedKeyArray = calloc (dynArray->size, sizeof (Key *));

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
		mmapMetaKey = (Key *) (metaPtr + (i * SIZEOF_KEY)); // new key location
		
		ELEKTRA_LOG_WARNING ("meta mmap location ptr: %p", (void *) (metaPtr + (i * SIZEOF_KEY)));
		ELEKTRA_LOG_WARNING ("meta old location ptr: %p", (void *) curMeta);
		ELEKTRA_LOG_WARNING ("%p key: %s, string: %s", (void *)curMeta, keyName (curMeta), keyString (curMeta));
		
		size_t keyNameSize = curMeta->keySize + curMeta->keyUSize;
		size_t keyValueSize = curMeta->dataSize;
		
		// move Key name
		if(curMeta->key)
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
			metaKeyValuePtr = dataPtr;
			dataPtr += keyValueSize;
		}
		else
		{
			metaKeyValuePtr = 0;
		}
		
		// move Key itself
		//memcpy (mmapMetaKey, curMeta, SIZEOF_KEY);
		mmapMetaKey->flags = curMeta->flags | KEY_FLAG_MMAP;
		mmapMetaKey->key = metaKeyNamePtr;
		mmapMetaKey->data.v = metaKeyValuePtr;
		mmapMetaKey->meta = 0;
		mmapMetaKey->ksReference = 0;
		mmapMetaKey->dataSize = curMeta->dataSize;
		mmapMetaKey->keySize = curMeta->keySize;
		mmapMetaKey->keyUSize = curMeta->keyUSize;
		
		dynArray->mappedKeyArray[i] = mmapMetaKey;
		ELEKTRA_LOG_WARNING ("NEW MMAP META KEY:");
		m_output_key(mmapMetaKey);
		
	}
	
	
	Key * cur;
	Key * mmapKey;
	void * keyNamePtr;
	void * keyValuePtr;
	size_t keyIndex = 0;
	ksRewind(keySet);
	while ((cur = ksNext (keySet)) != 0)
	{
		mmapKey = (Key *) (keyPtr + (keyIndex * SIZEOF_KEY));
		size_t keyNameSize = cur->keySize + cur->keyUSize;
		size_t keyValueSize = cur->dataSize;

		// move Key name
		if(cur->key)
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
			keyValuePtr = dataPtr;
			dataPtr += keyValueSize;
		}
		else
		{
			keyValuePtr = 0;
		}
		
		// write the meta KeySet
		KeySet * oldMeta = cur->meta;
		KeySet * newMeta = 0;
		if (cur->meta)
		{
			ELEKTRA_LOG_WARNING ("this key has meta info");
			newMeta = (KeySet *) metaKsPtr;
			//memcpy (newMeta, oldMeta, sizeof (KeySet));
			metaKsPtr += sizeof (KeySet);
			
			newMeta->flags = oldMeta->flags | KS_FLAG_MMAP;
			newMeta->array = (Key **) (metaKsPtr);
			
			keyRewindMeta(cur);
			size_t metaKeyIndex = 0;
			Key * mappedMetaKey = 0;
			const Key * metaKey;
			while ((metaKey = keyNextMeta (cur)) != 0)
			{
				// get address of mapped key and store it in the new array
				mappedMetaKey = dynArray->mappedKeyArray[find ((Key *) metaKey, dynArray)];
				ELEKTRA_LOG_WARNING ("mappedMetaKey: %p", (void *) mappedMetaKey);
				newMeta->array[metaKeyIndex] = mappedMetaKey;
				++(mappedMetaKey->ksReference);
				++metaKeyIndex;
			}
			newMeta->array[oldMeta->size] = 0;
			newMeta->alloc = oldMeta->alloc;
			newMeta->size = oldMeta->size;
			ksRewind(newMeta);
			metaKsPtr += (newMeta->alloc) * SIZEOF_KEY_PTR;
		}
		ELEKTRA_LOG_WARNING ("INSERT META INTO REAL KEY, HERE IS THE META KS:");
		m_output_keyset(newMeta);
		
		
		// move Key itself
		//memcpy (mmapKey, cur, SIZEOF_KEY);
		mmapKey->flags = cur->flags | KEY_FLAG_MMAP;
		mmapKey->key = keyNamePtr;
		mmapKey->keySize = cur->keySize;
		mmapKey->keyUSize = cur->keyUSize;
		mmapKey->data.v = keyValuePtr;
		mmapKey->dataSize = cur->dataSize;
		mmapKey->meta = newMeta;
		mmapKey->ksReference = 1;
		
		ELEKTRA_LOG_WARNING ("wrote new Key and meta KS is at: %p", (void *) newMeta);
		
		// write the Key pointer into the KeySet array
		//memcpy (++curKsArrayPtr, &mmapKey, SIZEOF_KEY_PTR);
		((Key **)ksArrayPtr)[keyIndex] = mmapKey;
		
		//m_output_key(mmapKey);
		++keyIndex;
	}
	
	//memcpy (ksPtr, keySet, SIZEOF_KEYSET);
	ksPtr->flags = keySet->flags | KS_FLAG_MMAP;
	ksPtr->array = (Key **) ksArrayPtr;
	ksPtr->array[keySet->size] = 0;
 	ksPtr->alloc = keySet->alloc;
 	ksPtr->size = keySet->size;
	ksRewind (ksPtr);
	//m_output_keyset (ksPtr);
}

static void elektraMmapstorageWrite (char * mappedRegion, KeySet * keySet, MmapHeader * mmapHeader, DynArray * dynArray)
{
	// multiple options for writing the KeySet:
	//		* fwrite () directly from the structs (needs multiple fwrite () calls)
	//		* memcpy () to continuous region and then fwrite () only once
	//		* use mmap to write to temp file and msync () after all data is written, then rename file
	mmapHeader->addr = mappedRegion;
	memcpy (mappedRegion, mmapHeader, SIZEOF_MMAPHEADER);
	
	KeySet * ksPtr = (KeySet *) (mappedRegion + SIZEOF_MMAPHEADER);

	if (keySet->size < 1)
	{
		// TODO: review mpranj
		memcpy (ksPtr, keySet, SIZEOF_KEYSET);
		ksPtr->flags = keySet->flags | KS_FLAG_MMAP;
		ksPtr->array = 0;
		ksPtr->alloc = 0;
		ksPtr->size = 0;
		ksPtr->cursor = 0;
		ksPtr->current = 0;
		return;
	}

	writeKeySet (mmapHeader, keySet, ksPtr, dynArray);
}

static void mmapToKeySet (char * mappedRegion, KeySet * returned)
{
	KeySet * keySet = (KeySet *) (mappedRegion + SIZEOF_MMAPHEADER);
	returned->array = keySet->array;
	returned->size 	= keySet->size;
	returned->alloc = keySet->alloc;
	//ksRewind(returned); // cursor = 0; current = 0
	returned->cursor = 0;
	returned->current = 0;
	//returned->flags = keySet->flags;
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

int elektraMmapstorageGet (Plugin * handle, KeySet * returned, Key * parentKey)
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

	if ((fp = elektraMmapstorageOpenFile (parentKey, "r+", errnosave)) == 0)
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
	{
// 		KeySet * empty = ksNew (0, KS_END);
// 		handle->kdbSet (handle, empty, parentKey);
		// TODO: fix this
// 		addr = mmapAddr (fp);
// 		mappedRegion = elektraMmapstorageMapFile (addr, fp, sbuf.st_size, MAP_SHARED | MAP_FIXED, parentKey, errnosave);
		ELEKTRA_LOG_WARNING ("FAILED to read mmap fixed address, NOT IMPLEMENTED");
		exit(128);
		//mappedRegion = elektraMmapstorageMapFile ((void *) 0, fp, sbuf.st_size, MAP_PRIVATE, parentKey, errnosave);
	}
	else
	{
		ELEKTRA_LOG_WARNING ("MMAP addr to map: %p", (void *) addr);
		mappedRegion = elektraMmapstorageMapFile (addr, fp, sbuf.st_size, MAP_PRIVATE | MAP_FIXED, parentKey, errnosave);
		ELEKTRA_LOG_WARNING ("mappedRegion ptr: %p", (void *) mappedRegion);
	}
	
	if (mappedRegion == MAP_FAILED)
	{
		fclose (fp);
		ELEKTRA_LOG ("mappedRegion == MAP_FAILED");
		return -1;
	}
	ELEKTRA_LOG_WARNING ("mappedRegion size: %zu", sbuf.st_size);
	ELEKTRA_LOG_WARNING ("mappedRegion ptr: %p", (void *) mappedRegion);

	ksClose (returned);
	//ksClear (returned);
	mmapToKeySet (mappedRegion, returned);
	
	ksRewind(returned);
	Key * cur;
	while((cur = ksNext(returned)) != 0)
	{
		keyIncRef(cur);
	}
	//m_output_keyset (returned);

	fclose (fp);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// set all keys
	//m_output_keyset (returned);
	
	int errnosave = errno;
	FILE * fp;

	if ((fp = elektraMmapstorageOpenFile (parentKey, "w+", errnosave)) == 0)
	{
		return -1;
	}
	
	DynArray dynArray;
	dynArray.keyArray = calloc (1, sizeof (Key *));
	dynArray.mappedKeyArray = 0;
	dynArray.size = 0;
	dynArray.alloc = 1;

	// TODO: calculating mmap size not needed if using fwrite() instead of mmap to write to file
	MmapHeader mmapHeader = elektraMmapstorageDataSize (returned, &dynArray);
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
	
	elektraMmapstorageWrite (mappedRegion, returned, &mmapHeader, &dynArray);
	//msync ((void *) mappedRegion, mmapHeader.mmapSize, MS_SYNC);
	ksClose (returned);
	
	// all data is written, further changes need to be copy-on-write
	mappedRegion = elektraMmapstorageMapFile ((void *) mappedRegion, fp, mmapHeader.mmapSize, MAP_PRIVATE | MAP_FIXED, parentKey, 
errnosave);
	ELEKTRA_LOG_WARNING ("mappedRegion ptr: %p", (void *) mappedRegion);
	if (mappedRegion == MAP_FAILED)
	{
		fclose (fp);
		ELEKTRA_LOG ("could not remap to MAP_PRIVATE");
		return -1;
	}
	
	mmapToKeySet (mappedRegion, returned);
	//m_output_keyset (returned);
	
	
	if (dynArray.keyArray)
		elektraFree (dynArray.keyArray);
	if (dynArray.mappedKeyArray)
		elektraFree (dynArray.mappedKeyArray);
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
