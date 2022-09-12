/**
 * @file
 *
 * @brief Source for file plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "file.h"
#include <fcntl.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>


int elektraFileGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/file"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/file", ELEKTRA_KEY_VALUE, "file plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/file/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/file/exports/get", ELEKTRA_KEY_FUNC, elektraFileGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/file/exports/set", ELEKTRA_KEY_FUNC, elektraFileSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/file/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; // success
	}

	short binary = 0;
	short info = 0;
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * lookup = elektraKeysetLookupByName (config, "/info", ELEKTRA_KDB_O_NONE);
	if (lookup) info = 1;
	lookup = elektraKeysetLookupByName (config, "/binary", ELEKTRA_KDB_O_NONE);
	if (lookup) binary = 1;

	const char * fileName = elektraKeyString (parentKey);

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

	ElektraKey * key = elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_END);
	if (binary)
	{
		elektraKeySetBinary (key, (const void *) buffer, (size_t) fileSize);
	}
	else
	{
		buffer[fileSize] = '\0';
		elektraKeySetString (key, (char *) buffer);
	}
	if (info)
	{
		unsigned long long maxValue = UINT64_MAX;
		size_t maxChars = snprintf (NULL, 0, "%llu", maxValue);
		char tmp[maxChars + 1]; //
		snprintf (tmp, sizeof (tmp), "%lld", fileSize);
		elektraKeySetMeta (key, "info/size", tmp);
		elektraKeySetMeta (key, "info/ctime", ctime (&sb.st_ctime));
		elektraKeySetMeta (key, "info/atime", ctime (&sb.st_atime));
		elektraKeySetMeta (key, "info/mtime", ctime (&sb.st_mtime));
		snprintf (tmp, sizeof (tmp), "%ld", (long) sb.st_uid);
		elektraKeySetMeta (key, "info/uid", tmp);
		snprintf (tmp, sizeof (tmp), "%ld", (long) sb.st_gid);
		elektraKeySetMeta (key, "info/gid", tmp);
		snprintf (tmp, sizeof (tmp), "%o", (unsigned int) sb.st_mode);
		elektraKeySetMeta (key, "info/mode", tmp);
		snprintf (tmp, sizeof (tmp), "%ld", (long) sb.st_ino);
		elektraKeySetMeta (key, "info/inode", tmp);
	}
	elektraKeysetAppendKey (returned, key);

	elektraFree (buffer);
	return 1; // success
}

int elektraFileSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional
	const ElektraKey * key = elektraKeysetLookup (returned, parentKey, ELEKTRA_KDB_O_NONE);
	if (!key) return 0;
	const char * fileName = elektraKeyString (parentKey);

	FILE * fp = NULL;
	fp = fopen (fileName, "wb");
	if (!fp)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to open %s for writing. Reason: %s", fileName, strerror (errno));
		return -1;
	}
	ssize_t svalueSize = elektraKeyGetValueSize (key);
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
	if (!elektraKeyIsBinary (key))
	{
		elektraKeyGetString (key, (char *) value, valueSize);
		valueSize -= 1; // don't write the null terminator to the file
	}
	else
	{
		elektraKeyGetBinary (key, value, valueSize);
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

