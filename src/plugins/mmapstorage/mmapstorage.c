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

#include <fcntl.h>		// open()
#include <errno.h>
#include <unistd.h>		// close()
#include <sys/mman.h>	// mmap()
#include <sys/stat.h>	// stat()

#define SIZEOF_KEY			(sizeof (Key))
#define SIZEOF_KEY_PTR		(sizeof (Key *))
#define SIZEOF_KEYSET		(sizeof (KeySet))
#define SIZEOF_KEYSET_PTR	(sizeof (KeySet *))



static int elektraMmapstorageOpenFile (int fd, Key * parentKey, int errnosave)
{
	ELEKTRA_LOG ("opening file %s", keyString (parentKey));

	// TODO: arbitrarily chosen to use 0644 here, fix later
	if ((fd = open (keyString (parentKey), O_RDWR | O_CREAT , 0644)) == -1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error opening file %s", keyString (parentKey));
		return -1;
	}
	return fd;
}

static int elektraMmapstorageTruncateFile (int fd, size_t mmapsize, Key * parentKey, int errnosave)
{
	ELEKTRA_LOG ("truncating file %s", keyString (parentKey));

	if ((ftruncate (fd, mmapsize)) == -1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error truncating file %s", keyString (parentKey));
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

static char * elektraMmapstorageMapFile (int fd, size_t mmapsize, Key * parentKey, int errnosave)
{
	ELEKTRA_LOG ("mapping file %s", keyString (parentKey));

	// TODO: fix magic mmap address
	// TODO: don't use MAP_FIXED
	// 0x E L E K T R A
	char * mappedRegion = mmap ((void *) 0x31337000000, mmapsize, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, fd, 0);
	if (mappedRegion == MAP_FAILED) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error mapping file %s", keyString (parentKey));
		return MAP_FAILED;
	}
	return mappedRegion;
}

static size_t elektraMmapstorageDataSize (KeySet * returned)
{
	Key * cur;
	ksRewind(returned);
	size_t dynamicDataSize = 0;
	while ((cur = ksNext (returned)) != 0)
	{
		dynamicDataSize += (keyGetNameSize(cur) + keyGetValueSize(cur));
	}

	size_t keyArraySize = (returned->size) * SIZEOF_KEY;
	size_t keyPtrArraySize = (returned->size) * SIZEOF_KEY_PTR;
	size_t mmapsize = SIZEOF_KEYSET + keyArraySize + dynamicDataSize + keyPtrArraySize;

	return mmapsize;
}

static void elektraMmapstorageWriteKeySet (KeySet * keySet, char * mappedRegion)
{
	if (keySet->size < 1)
	{
		// TODO: reduce this code duplication
		// move KeySet itself to the mapped region
		keySet->flags |= KS_FLAG_MMAP;
		memcpy (mappedRegion, keySet, SIZEOF_KEYSET);
		ksDel(keySet);
		return;
	}

	// TODO: save all Keys of KeySet to mapped region
	size_t keyArraySize = (keySet->size) * SIZEOF_KEY;
	size_t keyPtrArraySize = (keySet->size) * SIZEOF_KEY_PTR;

	size_t dataOffset = SIZEOF_KEYSET + keyArraySize; // ptr to start of DATA block
	char * dataNextFreeBlock = mappedRegion+dataOffset;


	Key * cur;
	Key ** mappedKeys = elektraMalloc (keyPtrArraySize);
	size_t keyIndex = 0;
	ksRewind(keySet);
	while ((cur = ksNext (keySet)) != 0)
	{
		ssize_t keyNameSize = keyGetNameSize(cur);
		ssize_t keyValueSize = keyGetValueSize(cur);

		// move Key name
		memcpy (dataNextFreeBlock, keyName(cur), keyNameSize);
		keySetName(cur, dataNextFreeBlock);
		dataNextFreeBlock += keyNameSize;

		// move Key value
		memcpy (dataNextFreeBlock, keyValue(cur), keyValueSize);
		keySetRaw(cur, dataNextFreeBlock, keyValueSize);
		dataNextFreeBlock += keyValueSize;

		// move Key itself
		void * mmapKey = mappedRegion + SIZEOF_KEYSET + (keyIndex * SIZEOF_KEY);
		cur->flags |= KEY_FLAG_MMAP;
		memcpy (mmapKey, cur, SIZEOF_KEY);

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
	memcpy (mappedRegion, keySet, SIZEOF_KEYSET);
	ksDel(keySet);
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
	int fd = -1;

	if ((fd = elektraMmapstorageOpenFile (fd, parentKey, errnosave)) == -1)
	{
		return -1;
	}

	struct stat sbuf;
	if (elektraMmapstorageStat (&sbuf, parentKey, errnosave) != 1)
	{
		close (fd);
		return -1;
	}

	char * mappedRegion = elektraMmapstorageMapFile (fd, sbuf.st_size, parentKey, errnosave);
	if (mappedRegion == MAP_FAILED)
	{
		close (fd);
		ELEKTRA_LOG ("mappedRegion == MAP_FAILED");
		return -1;
	}
	ELEKTRA_LOG ("mappedRegion size: %lld", sbuf.st_size);

	ksCopy(returned, (KeySet *) mappedRegion);

	//munmap(mappedRegion, sbuf.st_size);
	//close(fd);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	// set all keys

	int errnosave = errno;
	int fd = -1;

	if ((fd = elektraMmapstorageOpenFile (fd, parentKey, errnosave)) == -1)
	{
		return -1;
	}

	size_t mmapsize = elektraMmapstorageDataSize (returned);

	if (elektraMmapstorageTruncateFile (fd, mmapsize, parentKey, errnosave) != 1)
	{
		close (fd);
		return -1;
	}

	char * mappedRegion = elektraMmapstorageMapFile (fd, mmapsize, parentKey, errnosave);
	if (mappedRegion == MAP_FAILED)
	{
		close (fd);
		ELEKTRA_LOG ("mappedRegion == MAP_FAILED");
		return -1;
	}

	elektraMmapstorageWriteKeySet (ksDup(returned), mappedRegion);

	ksCopy(returned, (KeySet *) mappedRegion);

	//munmap(mappedRegion, mmapsize);
	//close(fd);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageError (Plugin * handle, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
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
