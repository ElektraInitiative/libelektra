/**
 * @file
 *
 * @brief Source for cache plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#define _XOPEN_SOURCE 600

#include "cache.h"

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbmodule.h>
#include <kdbprivate.h>

#include <fcntl.h>     // access()
#include <ftw.h>       // nftw()
#include <stdint.h>    // nftw()
#include <stdio.h>     // rename(), snprintf()
#include <stdlib.h>    // nftw(), getenv()
#include <string.h>    // nftw()
#include <sys/stat.h>  // elektraMkdirParents
#include <sys/time.h>  // gettimeofday()
#include <sys/types.h> // elektraMkdirParents
#include <unistd.h>    // access()

#define KDB_CACHE_STORAGE "mmapstorage"
#define POSTFIX_SIZE 50
#define MAX_FD_USED 32

typedef enum
{
	modeFile = 0,
	modeDirectory
} PathMode;

typedef struct _cacheHandle CacheHandle;

struct _cacheHandle
{
	ElektraKeyset * modules;
	ElektraKey * cachePath;
	Plugin * resolver;
	Plugin * cacheStorage;
};

static char * elektraStrConcat (const char * a, const char * b)
{
	size_t len = strlen (a) + strlen (b) + 1;
	char * ret = elektraMalloc (len);
	ret = strcpy (ret, a);
	ret = strcat (ret, b);
	return ret;
}

static int resolveCacheDirectory (Plugin * handle, CacheHandle * ch, ElektraKey * errorKey)
{
	ElektraKeyset * resolverConfig;
	char * cacheDir = getenv ("XDG_CACHE_HOME");
	if (cacheDir)
	{
		cacheDir = elektraStrConcat (cacheDir, "/elektra");
		ch->cachePath = elektraKeyNew ("system:/elektracache", ELEKTRA_KEY_END);
		resolverConfig = elektraKeysetNew (5, elektraKeyNew ("system:/path", ELEKTRA_KEY_VALUE, cacheDir, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraFree (cacheDir);
	}
	else
	{
		ch->cachePath = elektraKeyNew ("user:/elektracache", ELEKTRA_KEY_END);
		resolverConfig = elektraKeysetNew (5, elektraKeyNew ("user:/path", ELEKTRA_KEY_VALUE, "/.cache/elektra", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}

	ch->resolver = elektraPluginOpen (KDB_RESOLVER, ch->modules, resolverConfig, ch->cachePath);
	if (!ch->resolver)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Open of plugin returned unsuccessfully: %s", KDB_RESOLVER);
		elektraModulesClose (ch->modules, 0);
		elektraKeysetDel (ch->modules);
		elektraKeyDel (ch->cachePath);
		elektraFree (ch);
		return -1;
	}
	ch->resolver->global = elektraPluginGetGlobalKeySet (handle);
	// resolve cache directory in user home
	ch->resolver->kdbGet (ch->resolver, 0, ch->cachePath);

	return 0;
}

static int loadCacheStoragePlugin (Plugin * handle, CacheHandle * ch, ElektraKey * errorKey)
{
	ElektraKeyset * mmapstorageConfig = elektraKeysetNew (0, ELEKTRA_KS_END);
	ch->cacheStorage = elektraPluginOpen (KDB_CACHE_STORAGE, ch->modules, mmapstorageConfig, ch->cachePath);
	if (!ch->cacheStorage)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Open of plugin returned unsuccessfully: %s", KDB_CACHE_STORAGE);
		elektraPluginClose (ch->resolver, 0);
		elektraModulesClose (ch->modules, 0);
		elektraKeysetDel (ch->modules);
		elektraKeyDel (ch->cachePath);
		elektraFree (ch);
		return -1;
	}
	ch->cacheStorage->global = elektraPluginGetGlobalKeySet (handle);

	return 0;
}

static int elektraMkdirParents (const char * pathname)
{
	if (mkdir (pathname, KDB_FILE_MODE | KDB_DIR_MODE) == -1)
	{
		if (errno != ENOENT)
		{
			// hopeless, give it up
			return -1;
		}

		// last part of filename component (basename)
		char * p = strrchr (pathname, '/');

		/* nothing found */
		if (p == NULL)
		{
			// set any errno, corrected in
			// elektraAddErrnoText
			errno = E2BIG;
			return -1;
		}

		/* absolute path */
		if (p == pathname)
		{
			// set any errno, corrected in
			// elektraAddErrnoText
			errno = EINVAL;
			return -1;
		}

		/* Cut path at last /. */
		*p = 0;

		/* Now call ourselves recursively */
		if (elektraMkdirParents (pathname) == -1)
		{
			// do not yield an error, was already done
			// before
			*p = '/';
			return -1;
		}

		/* Restore path. */
		*p = '/';

		if (mkdir (pathname, KDB_FILE_MODE | KDB_DIR_MODE) == -1)
		{
			return -1;
		}
	}

	return 0;
}

static char * elektraGenTempFilename (char * cacheFileName)
{
	char * tmpFile = NULL;
	size_t tmpFilenameSize = 0;

	size_t cacheFileNameSize = strlen (cacheFileName);
	tmpFilenameSize = cacheFileNameSize + POSTFIX_SIZE;
	tmpFile = elektraCalloc (tmpFilenameSize);
	strncpy (tmpFile, cacheFileName, cacheFileNameSize + 1);

	struct timeval tv;
	memset (&tv, 0, sizeof (struct timeval));
	gettimeofday (&tv, 0);
	snprintf (tmpFile + cacheFileNameSize, tmpFilenameSize - cacheFileNameSize, ".%d:%ld." ELEKTRA_TIME_USEC_F ".tmp", getpid (),
		  tv.tv_sec, tv.tv_usec);
	return tmpFile;
}

static char * kdbCacheFileName (CacheHandle * ch, ElektraKey * parentKey, PathMode mode)
{
	char * cacheFileName = 0;
	const char * directory = elektraKeyString (ch->cachePath);
	ELEKTRA_LOG_DEBUG ("cache dir: %s", directory);
	if (mode == modeDirectory) return elektraStrDup (directory);

	const char * name = elektraKeyName (parentKey);
	elektraNamespace ns = elektraKeyGetNamespace (parentKey);
	ELEKTRA_LOG_DEBUG ("mountpoint name: %s", name);
	if (ns != ELEKTRA_NS_DEFAULT)
	{
		cacheFileName = elektraStrConcat (directory, "/backend");
		char * tmp = cacheFileName;
		cacheFileName = elektraStrConcat (cacheFileName, name);
		elektraFree (tmp);
	}
	else if (elektraStrCmp (elektraKeyString (parentKey), "default") == 0)
	{
		cacheFileName = elektraStrConcat (directory, "/default");
	}
	else
	{
		ELEKTRA_LOG_WARNING ("invalid mountpoint for caching: %s", name);
		ELEKTRA_ASSERT (0 != 0, "invalid mountpoint for caching");
	}

	if (cacheFileName)
	{
		if (access (cacheFileName, O_RDWR) != 0)
		{
			elektraMkdirParents (cacheFileName);
		}

		char * tmp = cacheFileName;
		if (elektraKeyGetMeta (parentKey, "cascading"))
		{
			cacheFileName = elektraStrConcat (cacheFileName, "/cache_cascading.mmap");
		}
		else
		{
			cacheFileName = elektraStrConcat (cacheFileName, "/cache.mmap");
		}
		elektraFree (tmp);
		ELEKTRA_LOG_DEBUG ("cache file: %s", cacheFileName);
	}

	return cacheFileName;
}

static int unlinkCacheFiles (const char * fpath, const struct stat * sb ELEKTRA_UNUSED, int tflag ELEKTRA_UNUSED,
			     struct FTW * ftwbuf ELEKTRA_UNUSED)
{
	ELEKTRA_LOG_DEBUG ("UNLINKING cache file: %s", fpath);
	remove (fpath);
	return 0;
}

int elektraCacheOpen (Plugin * handle, ElektraKey * errorKey)
{
	// plugin initialization logic
	// this function is optional
	ELEKTRA_LOG_DEBUG ("cache open");
	CacheHandle * ch = elektraMalloc (sizeof (CacheHandle));

	ch->modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (ch->modules, 0);

	if (resolveCacheDirectory (handle, ch, errorKey) == -1) return ELEKTRA_PLUGIN_STATUS_ERROR;
	if (loadCacheStoragePlugin (handle, ch, errorKey) == -1) return ELEKTRA_PLUGIN_STATUS_ERROR;

	elektraPluginSetData (handle, ch);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCacheClose (Plugin * handle, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	CacheHandle * ch = elektraPluginGetData (handle);
	if (ch)
	{
		elektraPluginClose (ch->resolver, 0);
		elektraPluginClose (ch->cacheStorage, 0);

		elektraModulesClose (ch->modules, 0);
		elektraKeysetDel (ch->modules);
		elektraKeyDel (ch->cachePath);

		elektraFree (ch);
		elektraPluginSetData (handle, 0);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCacheGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/cache") == 0)
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/cache", ELEKTRA_KEY_VALUE, "cache plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/cache/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/cache/exports/open", ELEKTRA_KEY_FUNC, elektraCacheOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/cache/exports/close", ELEKTRA_KEY_FUNC, elektraCacheClose, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/cache/exports/get", ELEKTRA_KEY_FUNC, elektraCacheGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/cache/exports/set", ELEKTRA_KEY_FUNC, elektraCacheSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/cache/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys
	CacheHandle * ch = elektraPluginGetData (handle);
	if (ch->cacheStorage->global == 0)
	{
		ch->cacheStorage->global = elektraPluginGetGlobalKeySet (handle);
	}

	if (!elektraStrCmp (elektraKeyString (elektraKeyGetMeta (parentKey, "cache/clear")), "1"))
	{
		// clear all caches
		ElektraKey * cacheFile = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
		char * cacheFileName = kdbCacheFileName (ch, cacheFile, modeDirectory);
		ELEKTRA_LOG_DEBUG ("CLEAR CACHES path: %s", cacheFileName);

		elektraKeySetString (cacheFile, cacheFileName);
		nftw (cacheFileName, unlinkCacheFiles, MAX_FD_USED, FTW_DEPTH);
		elektraFree (cacheFileName);
		elektraKeyDel (cacheFile);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	// construct cache file name from parentKey (which stores the mountpoint from mountGetMountpoint)
	ElektraKey * cacheFile = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	char * cacheFileName = kdbCacheFileName (ch, cacheFile, modeFile);
	ELEKTRA_ASSERT (cacheFileName != 0, "Could not construct cache file name.");
	ELEKTRA_LOG_DEBUG ("CACHE get cacheFileName: %s, parentKey: %s, %s", cacheFileName, elektraKeyName (parentKey), elektraKeyString (parentKey));

	// load cache from storage
	elektraKeySetString (cacheFile, cacheFileName);
	elektraFree (cacheFileName);

	// not the whole global keyset is cached
	// -> backup existing data
	ElektraKeyset * global = ch->cacheStorage->global;
	ch->cacheStorage->global = elektraKeysetNew (0, ELEKTRA_KS_END);

	// now we load the cache
	int result = ch->cacheStorage->kdbGet (ch->cacheStorage, returned, cacheFile);

	// extract the cached parts from the cache result
	ElektraKey * cacheCutpoint = elektraKeyNew ("system:/elektra/cache", ELEKTRA_KEY_END);   // internal cache data
	ElektraKey * cachedCutpoint = elektraKeyNew ("system:/elektra/cached", ELEKTRA_KEY_END); // other data that requests caching

	if (global != NULL)
	{
		ElektraKeyset * cut = elektraKeysetCut (ch->cacheStorage->global, cacheCutpoint);
		elektraKeysetAppend (global, cut);
		elektraKeysetDel (cut);

		cut = elektraKeysetCut (ch->cacheStorage->global, cachedCutpoint);
		elektraKeysetAppend (global, cut);
		elektraKeysetDel (cut);
	}

	elektraKeyDel (cacheCutpoint);
	elektraKeyDel (cachedCutpoint);

	// delete the rest and restore global keyset
	elektraKeysetDel (ch->cacheStorage->global);
	ch->cacheStorage->global = global;

	if (result == ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		elektraKeyDel (cacheFile);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	elektraKeyDel (cacheFile); // TODO: maybe propagate errors?
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

int elektraCacheSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	// set all keys
	// this function is optional
	CacheHandle * ch = elektraPluginGetData (handle);
	if (ch->cacheStorage->global == 0)
	{
		ch->cacheStorage->global = elektraPluginGetGlobalKeySet (handle);
	}

	if (elektraPluginGetGlobalKeySet (handle) == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE; // TODO: do we fail silently here?
	}

	// construct cache file name from parentKey (which stores the mountpoint from mountGetMountpoint)
	ElektraKey * cacheFile = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	char * cacheFileName = kdbCacheFileName (ch, cacheFile, modeFile);
	ELEKTRA_ASSERT (cacheFileName != 0, "Could not construct cache file name.");
	ELEKTRA_LOG_DEBUG ("CACHE set cacheFileName: %s, parentKey: %s, %s", cacheFileName, elektraKeyName (parentKey), elektraKeyString (parentKey));

	char * tmpFile = elektraGenTempFilename (cacheFileName);
	ELEKTRA_ASSERT (tmpFile != 0, "Could not construct temp file name.");
	ELEKTRA_LOG_DEBUG ("tmpFile: %s", tmpFile);

	// write cache to temp file
	elektraKeySetString (cacheFile, tmpFile);

	// don't cache the whole global keyset
	ElektraKey * cacheCutpoint = elektraKeyNew ("system:/elektra/cache", ELEKTRA_KEY_END);   // internal cache data
	ElektraKey * cachedCutpoint = elektraKeyNew ("system:/elektra/cached", ELEKTRA_KEY_END); // other data that requests caching

	ElektraKeyset * global = ch->cacheStorage->global;

	ch->cacheStorage->global = elektraKeysetCut (global, cacheCutpoint);

	ElektraKeyset * cut = elektraKeysetCut (global, cachedCutpoint);
	elektraKeysetAppend (ch->cacheStorage->global, cut);
	elektraKeysetDel (cut);

	elektraKeyDel (cacheCutpoint);
	elektraKeyDel (cachedCutpoint);

	// now we can store the cache
	int result = ch->cacheStorage->kdbSet (ch->cacheStorage, returned, cacheFile);

	// restore global keyset
	elektraKeysetAppend (global, ch->cacheStorage->global);
	elektraKeysetDel (ch->cacheStorage->global);
	ch->cacheStorage->global = global;

	if (result == ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		if (rename (tmpFile, cacheFileName) == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not rename file. Reason: %s", strerror (errno));
			goto error;
		}

		elektraFree (cacheFileName);
		elektraFree (tmpFile);
		elektraKeyDel (cacheFile);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

error:
	elektraFree (cacheFileName);
	elektraFree (tmpFile);
	elektraKeyDel (cacheFile);
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("cache",
		ELEKTRA_PLUGIN_OPEN,	&elektraCacheOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCacheClose,
		ELEKTRA_PLUGIN_GET,	&elektraCacheGet,
		ELEKTRA_PLUGIN_SET,	&elektraCacheSet,
		ELEKTRA_PLUGIN_END);
}
