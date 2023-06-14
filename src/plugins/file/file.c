/**
 * @file
 *
 * @brief Source for file plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./file.h"
#include <elektra/core/errors.h>
#include <fcntl.h>
#include <internal/macros/attributes.h>
#include <internal/utility/alloc.h>
#include <internal/utility/compare.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>


int elektraFileGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/file"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/file", KEY_VALUE, "file plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/file/exports", KEY_END),
			       keyNew ("system:/elektra/modules/file/exports/get", KEY_FUNC, elektraFileGet, KEY_END),
			       keyNew ("system:/elektra/modules/file/exports/set", KEY_FUNC, elektraFileSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/file/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	short binary = 0;
	short info = 0;
	KeySet * config = elektraPluginGetConfig (handle);
	Key * lookup = ksLookupByName (config, "/info", KDB_O_NONE);
	if (lookup) info = 1;
	lookup = ksLookupByName (config, "/binary", KDB_O_NONE);
	if (lookup) binary = 1;

	const char * fileName = keyString (parentKey);

	struct stat sb;
	if (stat (fileName, &sb) == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to stat file %s, aborting. Reason: %s", fileName, strerror (errno));
		return -1;
	}

	long long fileSize = (long long) sb.st_size;

	unsigned char * buffer = NULL;
	if (!binary)
		buffer = elektraMalloc (fileSize + 1); // fileSize + null terminator
	else
		buffer = elektraMalloc (fileSize);

	if (!buffer)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		return -1;
	}

	FILE * fp = NULL;
	fp = fopen (fileName, "rb");
	if (!fp)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to open file %s. Reason: %s", fileName, strerror (errno));
		elektraFree (buffer);
		return -1;
	}


	// loop until until fileSize elements of size sizeof(char) are read
	long long bytesRead = 0;
	while (bytesRead < fileSize)
	{
		size_t bytes = fread (buffer + bytesRead, 1, (size_t) fileSize, fp);
		if (bytes == 0) break;
		bytesRead += bytes;
	}

	if (bytesRead < fileSize)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Failed to read %s completely. Got %lld of %lld bytes", fileName,
							 bytesRead, fileSize);
		elektraFree (buffer);
		fclose (fp);
		return -1;
	}
	fclose (fp);

	Key * key = keyNew (keyName (parentKey), KEY_END);
	if (binary)
	{
		keySetBinary (key, (const void *) buffer, (size_t) fileSize);
	}
	else
	{
		buffer[fileSize] = '\0';
		keySetString (key, (char *) buffer);
	}
	if (info)
	{
		unsigned long long maxValue = UINT64_MAX;
		size_t maxChars = snprintf (NULL, 0, "%llu", maxValue);
		char tmp[maxChars + 1]; //
		snprintf (tmp, sizeof (tmp), "%lld", fileSize);
		keySetMeta (key, "info/size", tmp);
		keySetMeta (key, "info/ctime", ctime (&sb.st_ctime));
		keySetMeta (key, "info/atime", ctime (&sb.st_atime));
		keySetMeta (key, "info/mtime", ctime (&sb.st_mtime));
		snprintf (tmp, sizeof (tmp), "%ld", (long) sb.st_uid);
		keySetMeta (key, "info/uid", tmp);
		snprintf (tmp, sizeof (tmp), "%ld", (long) sb.st_gid);
		keySetMeta (key, "info/gid", tmp);
		snprintf (tmp, sizeof (tmp), "%o", (unsigned int) sb.st_mode);
		keySetMeta (key, "info/mode", tmp);
		snprintf (tmp, sizeof (tmp), "%ld", (long) sb.st_ino);
		keySetMeta (key, "info/inode", tmp);
	}
	ksAppendKey (returned, key);

	elektraFree (buffer);
	return 1; // success
}

int elektraFileSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	const Key * key = ksLookup (returned, parentKey, KDB_O_NONE);
	if (!key) return 0;
	const char * fileName = keyString (parentKey);

	FILE * fp = NULL;
	fp = fopen (fileName, "wb");
	if (!fp)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to open %s for writing. Reason: %s", fileName, strerror (errno));
		return -1;
	}
	ssize_t svalueSize = keyGetValueSize (key);
	if (svalueSize <= 0)
	{
		fclose (fp);
		return 0;
	}
	size_t valueSize = (size_t) svalueSize;
	unsigned char * value = elektraMalloc (valueSize);
	if (!value)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		fclose (fp);
		return -1;
	}
	if (!keyIsBinary (key))
	{
		keyGetString (key, (char *) value, valueSize);
		valueSize -= 1; // don't write the null terminator to the file
	}
	else
	{
		keyGetBinary (key, value, valueSize);
	}
	size_t bytesWritten = 0;
	while (bytesWritten < valueSize)
	{
		size_t bytes = fwrite (value, 1, valueSize, fp);
		if (bytes == 0) break;
		bytesWritten += bytes;
	}
	fclose (fp);
	elektraFree (value);

	if (bytesWritten < valueSize)
	{
		return -1;
	}

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("file",
		ELEKTRA_PLUGIN_GET,	&elektraFileGet,
		ELEKTRA_PLUGIN_SET,	&elektraFileSet,
		ELEKTRA_PLUGIN_END);
}

