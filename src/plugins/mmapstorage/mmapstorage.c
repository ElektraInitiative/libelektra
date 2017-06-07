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
	int fd;
	ELEKTRA_LOG ("opening file %s", keyString (parentKey));

	// TODO: arbitrarily chosen to use 0644 here, fix later
	if ((fd = open (keyString (parentKey), O_RDWR | O_CREAT , 0644)) == -1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error opening file %s", keyString (parentKey));
		return -1;
	}

	struct stat sbuf;
	if (stat(keyString (parentKey), &sbuf) == -1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error on stat() for file %s", keyString (parentKey));
		return -1;
	}

	char * mappedRegion;
	void * fixedAddr = (void *) 0x31337000000;
	mappedRegion = mmap (fixedAddr, sbuf.st_size, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, fd, 0);
	if (mappedRegion == (void *)-1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error mapping file %s", keyString (parentKey));
		close (fd);
		return -1;
	}

	returned = (KeySet *) (&(* mappedRegion));

//	size_t keySize = sizeof (Key);
//
//	for(size_t i = 0; i < rootKeySet->size; ++i)
//	{
//
//	}

	//munmap (mappedRegion, sbuf.st_size);
	//close (fd);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraMmapstorageSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional


	int errnosave = errno;
	int fd;
	ELEKTRA_LOG ("opening file %s", keyString (parentKey));

	// TODO: arbitrarily chosen to use 0644 here, fix later
	if ((fd = open (keyString (parentKey), O_RDWR | O_CREAT , 0644)) == -1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error opening file %s", keyString (parentKey));
		return -1;
	}

	ELEKTRA_LOG ("truncating file %s", keyString (parentKey));
	// TODO: truncate file to correct size
	size_t keySize = sizeof (Key);
	size_t keyArraySize = (returned->size) * keySize;
	size_t keySetSize = sizeof (KeySet);
	size_t mmapsize = keySetSize + keyArraySize + getpagesize ();
	if ((ftruncate (fd, mmapsize)) == -1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error truncating file %s", keyString (parentKey));
		close (fd);
		return -1;
	}

	char * mappedRegion;
	void * fixedAddr = (void *) 0x31337000000;
	mappedRegion = mmap (fixedAddr, mmapsize, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, fd, 0);
	if (mappedRegion == (void *)-1) {
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		ELEKTRA_LOG_WARNING ("error mapping file %s", keyString (parentKey));
		close (fd);
		return -1;
	}



	// TODO: save all Keys of KeySet to mapped region

	Key * cur;
	ksRewind(returned);
	size_t keyIndex = 0;
	size_t keyOffset = keySetSize;
	size_t dataOffset = keyOffset + keyArraySize; // ptr to start of DATA block
	char * dataNextFreeBlock = mappedRegion+dataOffset;

	Key ** mappedKeys = elektraMalloc(keyArraySize+1);
	while ((cur = ksNext (returned)) != 0)
	{
		// move key name
		memcpy (dataNextFreeBlock, (const void *) cur->key, cur->keySize); // TODO: use keyGetNameSize
		cur->key = dataNextFreeBlock;
		dataNextFreeBlock += cur->keySize;

		// move key value
		memcpy (dataNextFreeBlock, cur->data.v, cur->dataSize); // TODO: use keyGetValueSize
		cur->data.v = dataNextFreeBlock;
		dataNextFreeBlock += cur->dataSize;

		// move key itself
		void * mmapKey = mappedRegion+keyOffset+(keyIndex*keySize);
		memcpy (mmapKey, cur, keySize);

		mappedKeys[keyIndex] = mmapKey;
		++keyIndex;
	}
	mappedKeys[keyIndex] = 0;
	// copy key ptrs from array to DATA block
	memcpy (dataNextFreeBlock, mappedKeys, keyArraySize);
	returned->array = (Key **) dataNextFreeBlock;
	dataNextFreeBlock += keyArraySize;
	elektraFree (mappedKeys);

	// TODO: save KeySet int mapped region
	// TODO: update KeySet array!!!!!

	memcpy (mappedRegion, returned, keySetSize);

	//munmap (mappedRegion, mmapsize);
	//close (fd);
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
