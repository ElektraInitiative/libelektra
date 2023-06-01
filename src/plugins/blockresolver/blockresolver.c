/**
 * @file
 *
 * @brief Source for blockresolver plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./blockresolver.h"

#include <elektra/core/errors.h>
#include <elektra/type/types.h>
#include <elektra/plugin/invoke.h>

#include <internal/config.h>
#include <internal/macros/attributes.h>
#include <internal/resolver/shared.h>
#include <internal/utility/alloc.h>
#include <internal/utility/compare.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>


#define TV_MAX_DIGITS 26
#define BUFSIZE_MAX 1024

typedef struct
{
	char * tmpFile;
	char * realFile;
	char * identifier;
	time_t mtime;
	long startPos;
	long endPos;
	unsigned short getPass;
	unsigned short setPass;
} BlockData;

static int elektraResolveFilename (Key * parentKey, ElektraResolveTempfile tmpFile)
{
	int rc = 0;
	ElektraInvokeHandle * handle = elektraInvokeOpen ("resolver", 0, 0);
	if (!handle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	ElektraResolved * resolved = NULL;
	elektraResolveFileFunc resolveFunc = *(elektraResolveFileFunc *) elektraInvokeGetFunction (handle, "filename");

	if (!resolveFunc)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	elektraFreeResolvedFunc freeHandle = *(elektraFreeResolvedFunc *) elektraInvokeGetFunction (handle, "freeHandle");

	if (!freeHandle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	resolved = resolveFunc (keyGetNamespace (parentKey), keyString (parentKey), tmpFile, parentKey);

	if (!resolved)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	else
	{
		keySetString (parentKey, resolved->fullPath);
		freeHandle (resolved);
	}

RESOLVE_FAILED:
	elektraInvokeClose (handle, 0);
	return rc;
}

int elektraBlockresolverCheckFile (const char * filename ELEKTRA_UNUSED)
{
	return 1;
}

static const char * genTempFilename (void)
{
	struct timeval tv;
	gettimeofday (&tv, 0);
	const char * tmpFilePrefix = "/tmp/elektra_blockresolver_";
	char * tmpFile = elektraMalloc (strlen (tmpFilePrefix) + TV_MAX_DIGITS + 2);
	snprintf (tmpFile, strlen (tmpFilePrefix) + TV_MAX_DIGITS + 2, "%s%lu:" ELEKTRA_TIME_USEC_F, tmpFilePrefix, tv.tv_sec, tv.tv_usec);
	return tmpFile;
}

static int initData (Plugin * handle, Key * parentKey)
{
	BlockData * data = elektraPluginGetData (handle);
	if (!data)
	{
		data = elektraCalloc (sizeof (BlockData));
		elektraPluginSetData (handle, data);
		KeySet * config = elektraPluginGetConfig (handle);
		Key * key = ksLookupByName (config, "/identifier", KDB_O_NONE);
		if (!key) return -1;
		data->identifier = (char *) keyString (key);
		if (elektraResolveFilename (parentKey, ELEKTRA_RESOLVER_TEMPFILE_NONE) == -1)
		{
			return -1;
		}
		data->realFile = elektraStrDup (keyString (parentKey));
		struct stat buf;
		if (stat (data->realFile, &buf))
		{
			return -1;
		}
		data->mtime = buf.st_mtime;
		data->tmpFile = (char *) genTempFilename ();
		data->startPos = -1;
		data->endPos = -1;
		data->getPass = 0;
		data->setPass = 0;
	}
	return 0;
}

int elektraBlockresolverClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	BlockData * data = elektraPluginGetData (handle);
	if (data)
	{
		if (data->tmpFile)
		{
			unlink (data->tmpFile);
			elektraFree (data->tmpFile);
		}
		if (data->realFile) elektraFree (data->realFile);
		elektraFree (data);
	}
	elektraPluginSetData (handle, 0);
	return 1; // success
}

static long getBlockStart (FILE * fp, const char * identifier)
{
	long position = -1;
	char buffer[BUFSIZE_MAX];
	fseek (fp, 0, SEEK_SET);
	while (fgets (buffer, sizeof (buffer), fp))
	{
		if (!strncmp (buffer, identifier, strlen (identifier)))
		{
			if (!strcmp (buffer + strlen (identifier) + 1, "start\n"))
			{
				position = ftell (fp);
				break;
			}
			else
			{
				break;
			}
		}
	}
	return position;
}

static long getBlockEnd (FILE * fp, const char * identifier, long offset)
{
	if (offset < 0) return -1;
	long position = -1;
	char buffer[BUFSIZE_MAX];
	fseek (fp, offset, 0);
	while (fgets (buffer, sizeof (buffer), fp))
	{
		if (!strncmp (buffer, identifier, strlen (identifier)))
		{
			if (!strcmp (buffer + strlen (identifier) + 1, "stop\n"))
			{
				position = ftell (fp) - (long) strlen (buffer);
				break;
			}
			else
			{
				break;
			}
		}
	}
	return position;
}

static const char * getBlock (FILE * fp, const long startPos, const long endPos)
{
	fseek (fp, startPos, SEEK_SET);
	if (endPos <= startPos) return NULL;
	size_t blockSize = (size_t) (endPos - startPos);
	if (blockSize <= 0) return NULL;
	char * block = elektraMalloc (blockSize + 1);
	if (!block) return NULL;
	size_t read = fread (block, 1, blockSize, fp);
	if (read != blockSize)
	{
		elektraFree (block);
		return NULL;
	}
	block[read] = '\0';
	return block;
}

int elektraBlockresolverGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/blockresolver"))
	{
		KeySet * contract = ksNew (
			30,
			keyNew ("system:/elektra/modules/blockresolver", KEY_VALUE, "blockresolver plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/blockresolver/exports", KEY_END),
			keyNew ("system:/elektra/modules/blockresolver/exports/close", KEY_FUNC, elektraBlockresolverClose, KEY_END),
			keyNew ("system:/elektra/modules/blockresolver/exports/error", KEY_FUNC, elektraBlockresolverError, KEY_END),
			keyNew ("system:/elektra/modules/blockresolver/exports/get", KEY_FUNC, elektraBlockresolverGet, KEY_END),
			keyNew ("system:/elektra/modules/blockresolver/exports/set", KEY_FUNC, elektraBlockresolverSet, KEY_END),
			keyNew ("system:/elektra/modules/blockresolver/exports/commit", KEY_FUNC, elektraBlockresolverCommit, KEY_END),
			keyNew ("system:/elektra/modules/blockresolver/exports/checkfile", KEY_FUNC, elektraBlockresolverCheckFile,
				KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/blockresolver/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	int rc = initData (handle, parentKey);
	if (rc) return -1;

	int retVal = 0;
	BlockData * data = elektraPluginGetData (handle);
	keySetString (parentKey, data->tmpFile);
	FILE * fin = NULL;
	FILE * fout = NULL;
	char * block = NULL;

	if (data->getPass > 0)
	{
		struct stat buf;
		if (stat (data->realFile, &buf))
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to stat file %s\n. Reason: %s", data->realFile, strerror (errno));
			return -1;
		}
		if (buf.st_mtime == data->mtime) return 0;
	}

	fin = fopen (data->realFile, "r");
	if (!fin)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Couldn't open %s for reading. Reason: %s", data->realFile, strerror (errno));
		goto GET_CLEANUP;
	}

	data->startPos = getBlockStart (fin, data->identifier);
	if (data->startPos == -1) goto GET_CLEANUP;
	data->endPos = getBlockEnd (fin, data->identifier, data->startPos);
	if (data->endPos == -1)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Couldn't find end of block %s", data->identifier);
		retVal = -1;
		goto GET_CLEANUP;
	}
	block = (char *) getBlock (fin, data->startPos, data->endPos);
	if (!block)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Failed to extract block %s\n", data->identifier);
		retVal = -1;
		goto GET_CLEANUP;
	}
	fclose (fin);
	fin = NULL;
	fout = fopen (data->tmpFile, "w");
	if (!fout)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Couldn't open %s for writing. Reason: %s", data->tmpFile, strerror (errno));
		retVal = -1;
		goto GET_CLEANUP;
	}
	size_t blockSize = (size_t) (data->endPos - data->startPos);
	fwrite (block, 1, blockSize, fout);
	retVal = 1;
	++(data->getPass);
GET_CLEANUP:
	if (fin) fclose (fin);
	if (fout) fclose (fout);
	if (block) elektraFree (block);
	return retVal; // success
}

int elektraBlockresolverSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	BlockData * data = elektraPluginGetData (handle);
	if (!data) return -1;
	keySetString (parentKey, data->tmpFile);
	struct stat buf;
	if (stat (data->realFile, &buf))
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to stat file %s\n. Reason: %s", data->realFile, strerror (errno));
		return -1;
	}
	if (buf.st_mtime > data->mtime)
	{
		ELEKTRA_SET_CONFLICTING_STATE_ERRORF (parentKey, "File '%s' has been modified", data->realFile);
		return -1;
	}
	FILE * fout = NULL;
	FILE * fin = NULL;
	char * block = NULL;
	char * mergeFile = NULL;
	int retVal = -1;
	if (!data->setPass)
	{
		++(data->setPass);
		return 1;
	}
	else if (data->setPass == 1)
	{
		// commit phase
		mergeFile = (char *) genTempFilename ();
		fout = fopen (mergeFile, "w");
		if (!fout)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Couldn't open %s for writing. Reason: %s", data->realFile,
						     strerror (errno));
			goto SET_CLEANUP;
		}
		fin = fopen (data->realFile, "r");
		if (!fin)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Couldn't open %s for reading. Reason: %s", data->realFile,
						     strerror (errno));
			goto SET_CLEANUP;
		}
		block = (char *) getBlock (fin, 0, data->startPos);
		if (!block)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Failed to extract block before %s\n", data->identifier);
			goto SET_CLEANUP;
		}
		fwrite (block, 1, (size_t) data->startPos, fout);
		fseek (fin, 0, SEEK_END);
		elektraFree (block);
		block = NULL;
		size_t blockSize = (size_t) (ftell (fin) - data->endPos);
		block = (char *) getBlock (fin, data->endPos, ftell (fin));
		if (!block)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Failed to extract block after %s\n", data->identifier);
			goto SET_CLEANUP;
		}
		fclose (fin);
		fin = fopen (data->tmpFile, "r");
		if (!fin)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Couldn't open %s for reading Reason: %s", data->tmpFile, strerror (errno));
			goto SET_CLEANUP;
		}
		char buffer[BUFSIZE_MAX];
		size_t read = 0;
		while ((read = fread (buffer, 1, sizeof (buffer), fin)) > 0)
		{
			fwrite (buffer, 1, read, fout);
		}
		fwrite (block, 1, blockSize, fout);
		retVal = 1;
	}

SET_CLEANUP:
	if (fin) fclose (fin);
	if (fout) fclose (fout);
	if (block) elektraFree (block);

	if (retVal == 1)
	{
		if (rename (mergeFile, data->realFile) == -1) retVal = -1;
		elektraFree (mergeFile);
		mergeFile = NULL;
	}
	if (mergeFile) elektraFree (mergeFile);
	return retVal; // success
}

int elektraBlockresolverError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return 1; // success
}

int elektraBlockresolverCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return elektraBlockresolverSet (handle, returned, parentKey);
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("blockresolver",
	    ELEKTRA_PLUGIN_CLOSE,	&elektraBlockresolverClose,
	    ELEKTRA_PLUGIN_ERROR, &elektraBlockresolverError,
	    ELEKTRA_PLUGIN_GET,	&elektraBlockresolverGet,
	    ELEKTRA_PLUGIN_SET,	&elektraBlockresolverSet,
	    ELEKTRA_PLUGIN_COMMIT, &elektraBlockresolverCommit,
	    ELEKTRA_PLUGIN_END);
}

